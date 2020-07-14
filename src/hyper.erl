%% @doc: Implementation of HyperLogLog with bias correction as
%% described in the Google paper,
%% http://static.googleusercontent.com/external_content/untrusted_dlcp/
%% research.google.com/en//pubs/archive/40671.pdf
-module(hyper).

%%-compile(native).

-export([new/1, new/2, insert/2, insert_many/2]).
-export([union/1, union/2]).
-export([card/1, intersect_card/2]).
-export([to_json/1, from_json/1, from_json/2, precision/1, bytes/1, is_hyper/1]).
-export([compact/1, reduce_precision/2]).

-type precision() :: 4..16.
-type registers() :: any().

-record(hyper, {p :: precision(), registers :: {module(), registers()}}).

-type value() :: binary().
-type filter() :: #hyper{}.

-export_type([filter/0, precision/0, registers/0]).

%% Exported for testing
-export([run_of_zeroes/1]).

-define(DEFAULT_BACKEND, hyper_binary).

%%
%% API
%%

-spec new(precision()) -> filter().
new(P) ->
    new(P, ?DEFAULT_BACKEND).

-spec new(precision(), module()) -> filter().
new(P, Mod) when 4 =< P andalso P =< 16 andalso is_atom(Mod) ->
    #hyper{p = P, registers = {Mod, Mod:new(P)}}.

-spec is_hyper(filter()) -> boolean().
is_hyper(#hyper{}) ->
    true;
is_hyper(_) ->
    false.

-spec insert(value(), filter()) -> filter().
insert(Value, #hyper{registers = {Mod, Registers}, p = P} = Hyper)
    when is_binary(Value) ->
    Hash = crypto:hash(sha, Value),
    <<Index:P, RegisterValue:P/bitstring, _/bitstring>> = Hash,

    ZeroCount = run_of_zeroes(RegisterValue) + 1,

    %% Registers are only allowed to increase, implement by backend
    Hyper#hyper{registers = {Mod, Mod:set(Index, ZeroCount, Registers)}};
insert(_Value, _Hyper) ->
    error(badarg).

-spec insert_many([value()], filter()) -> filter().
insert_many(L, Hyper) ->
    lists:foldl(fun insert/2, Hyper, L).

-spec union([filter()]) -> filter().
union(Filters) when is_list(Filters) ->
    case lists:usort(lists:map(fun (#hyper{p = P, registers = {Mod, _}}) ->
                                       {P, Mod}
                               end,
                               Filters))
        of
      %% same P and backend
      [{_P, Mod}] ->
          Registers = lists:map(fun (#hyper{registers = {_, R}}) ->
                                        R
                                end,
                                Filters),
          [First | _] = Filters,
          First#hyper{registers = {Mod, Mod:max_merge(Registers)}};
      %% mixed P, but still must have same backend
      [{MinP, Mod} | _] ->
          FoldedFilters = lists:map(fun (#hyper{registers = {M, _}} = F) when M =:= Mod ->
                                            hyper:reduce_precision(MinP, F)
                                    end,
                                    Filters),
          union(FoldedFilters)
    end.

union(Small, Big) ->
    union([Small, Big]).

%% NOTE: use with caution, no guarantees on accuracy.
-spec intersect_card(filter(), filter()) -> float().
intersect_card(Left, Right) when Left#hyper.p =:= Right#hyper.p ->
    max(0.0, card(Left) + card(Right) - card(union(Left, Right))).

-spec card(filter()) -> float().
card(#hyper{registers = {Mod, Registers0}, p = P}) ->
    M = trunc(pow(2, P)),
    Registers = Mod:compact(Registers0),

    RegisterSum = Mod:register_sum(Registers),

    E = alpha(M) * pow(M, 2) / RegisterSum,
    Ep = case E =< 5 * M of
           true ->
               E - estimate_bias(E, P);
           false ->
               E
         end,
    V = Mod:zero_count(Registers),

    H = case V of
          0 ->
              Ep;
          _ ->
              M * math:log(M / V)
        end,
    case H =< hyper_const:threshold(P) of
      true ->
          H;
      false ->
          Ep
    end.

precision(#hyper{p = Precision}) ->
    Precision.

bytes(#hyper{registers = {Mod, Registers}}) ->
    Mod:bytes(Registers).

compact(#hyper{registers = {Mod, Registers}} = Hyper) ->
    Hyper#hyper{registers = {Mod, Mod:compact(Registers)}}.

reduce_precision(P, #hyper{p = OldP, registers = {Mod, Registers}} = Hyper)
    when P < OldP ->
    Hyper#hyper{p = P, registers = {Mod, Mod:reduce_precision(P, Registers)}};
reduce_precision(P, #hyper{p = P} = Filter) ->
    Filter.

%%
%% SERIALIZATION
%%

-spec to_json(filter()) -> any().
to_json(#hyper{p = P, registers = {Mod, Registers}}) ->
    Compact = Mod:compact(Registers),
    {[{<<"p">>, P},
      {<<"registers">>, base64:encode(zlib:gzip(Mod:encode_registers(Compact)))}]}.

-spec from_json(any()) -> filter().
from_json(Struct) ->
    from_json(Struct, ?DEFAULT_BACKEND).

-spec from_json(any(), module()) -> filter().
from_json({Struct}, Mod) ->
    P = proplists:get_value(<<"p">>, Struct),
    Bytes = zlib:gunzip(base64:decode(proplists:get_value(<<"registers">>, Struct))),
    Registers = Mod:decode_registers(Bytes, P),

    #hyper{p = P, registers = {Mod, Registers}}.

%%
%% HELPERS
%%

alpha(16) ->
    0.673;
alpha(32) ->
    0.697;
alpha(64) ->
    0.709;
alpha(M) ->
    0.7213 / (1 + 1.079 / M).

pow(X, Y) ->
    math:pow(X, Y).

run_of_zeroes(B) ->
    run_of_zeroes(1, B).

run_of_zeroes(I, B) ->
    case B of
      <<0:I, _/bitstring>> ->
          run_of_zeroes(I + 1, B);
      _ ->
          I - 1
    end.

estimate_bias(E, P) ->
    BiasVector = hyper_const:bias_data(P),
    EstimateVector = hyper_const:estimate_data(P),
    NearestNeighbours = nearest_neighbours(E, EstimateVector),

    lists:sum([element(Index, BiasVector) || Index <- NearestNeighbours]) /
      length(NearestNeighbours).

nearest_neighbours(E, Vector) ->
    Distances = lists:map(fun (Index) ->
                                  V = element(Index, Vector),
                                  {pow(E - V, 2), Index}
                          end,
                          lists:seq(1, size(Vector))),
    SortedDistances = lists:keysort(1, Distances),

    {_, Indexes} = lists:unzip(lists:sublist(SortedDistances, 6)),
    Indexes.

