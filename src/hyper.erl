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
-export([generate_unique/1]).

-type precision() :: 4..16.
-type registers() :: any().

-record(hyper, {p :: precision(), registers :: {module(), registers()}}).

-type value() :: binary().
-type filter() :: #hyper{}.

-export_type([filter/0, precision/0, registers/0]).

%% Exported for testing
-export([run_of_zeroes/1]).

-define(DEFAULT_BACKEND, hyper_binary).
-define(HLL_ALPHA_INF, 0.721347520444481703680). % constant for 0.5/ln(2)

%%
%% API
%%

-spec new(precision()) -> filter().
new(P) ->
    new(P, ?DEFAULT_BACKEND).

-spec new(precision(), module()) -> filter().
new(P, Mod) when 4 =< P andalso P =< 16 andalso is_atom(Mod) ->
    #hyper{p = P, registers = {Mod, hyper_register:new(Mod, P)}}.

-spec is_hyper(filter()) -> boolean().
is_hyper(#hyper{}) ->
    true;
is_hyper(_) ->
    false.

-spec insert(value(), filter()) -> filter().
insert(Value, #hyper{registers = {Mod, Registers}, p = P} = Hyper)
    when is_binary(Value) ->
    Hash = crypto:hash(sha, Value),
    <<Index:P, RegisterValue:(64 - P)/bitstring, _/bitstring>> = Hash,

    ZeroCount = run_of_zeroes(RegisterValue) + 1,

    %% Registers are only allowed to increase, implement by backend
    Hyper#hyper{registers = {Mod, hyper_register:set(Mod, Index, ZeroCount, Registers)}};
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
          First#hyper{registers = {Mod, hyper_register:max_merge(Mod, Registers)}};
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
    Qp1 = 65 - P,
    Registers = hyper_register:compact(Mod, Registers0),
    RegisterHisto = hyper_register:register_histogram(Mod, Registers),

    Z = M * tau(M - maps:get(Qp1, RegisterHisto,  0) / M),
    %TODO: drop after Q = 64 - P in histo before folding
    Z1 = lists:foldr(
        fun({_K, V}, Acc) -> (Acc + V) * 0.5 end,
        Z,
        lists:keysort(1, maps:to_list(maps:without([0, Qp1], RegisterHisto)))
    ),
    Zf = Z1 + M * sigma(maps:get(0, RegisterHisto, 0) / M),
    ?HLL_ALPHA_INF * M * M / Zf.

precision(#hyper{p = Precision}) ->
    Precision.

bytes(#hyper{registers = {Mod, Registers}}) ->
    hyper_register:bytes(Mod, Registers).

compact(#hyper{registers = {Mod, Registers}} = Hyper) ->
    Hyper#hyper{registers = {Mod, hyper_register:compact(Mod, Registers)}}.

reduce_precision(P, #hyper{p = OldP, registers = {Mod, Registers}} = Hyper)
    when P < OldP ->
    Hyper#hyper{p = P, registers = {Mod, hyper_register:reduce_precision(Mod, P, Registers)}};
reduce_precision(P, #hyper{p = P} = Filter) ->
    Filter.

%%
%% SERIALIZATION
%%

-spec to_json(filter()) -> any().
to_json(#hyper{p = P, registers = {Mod, Registers}}) ->
    Compact = hyper_register:compact(Mod, Registers),
    {[{<<"p">>, P},
      {<<"registers">>, base64:encode(zlib:gzip(hyper_register:encode_registers(Mod, Compact)))}]}.

-spec from_json(any()) -> filter().
from_json(Struct) ->
    from_json(Struct, ?DEFAULT_BACKEND).

-spec from_json(any(), module()) -> filter().
from_json({Struct}, Mod) ->
    P = proplists:get_value(<<"p">>, Struct),
    Bytes = zlib:gunzip(base64:decode(proplists:get_value(<<"registers">>, Struct))),
    Registers = hyper_register:decode_registers(Mod, Bytes, P),

    #hyper{p = P, registers = {Mod, Registers}}.

%%
%% HELPERS
%%

generate_unique(N) ->
    generate_unique(lists:usort(random_bytes(N)), N).

generate_unique(L, N) ->
    case length(L) of
      N ->
          L;
      Less ->
          generate_unique(lists:usort(random_bytes(N - Less) ++ L), N)
    end.

random_bytes(N) ->
    random_bytes([], N).

random_bytes(Acc, 0) ->
    Acc;
random_bytes(Acc, N) ->
    Int = rand:uniform(100000000000000),
    random_bytes([<<Int:64/integer>> | Acc], N - 1).

sigma(1.0) ->
    infinity;
sigma(X) ->
    sigma_sum(X, first, X, 1.0).

sigma_sum(Z, Z, _X, _Y) ->
    Z;
sigma_sum(Z, _Zp, X, Y) ->
    X1 = X * X,
    Z1 = (X1* Y) + Z,
    sigma_sum(Z1, Z, X1, Y + Y).

tau(0.0) ->
    0.0;
tau(1.0) ->
    0.0;
tau(X) ->
    tau_sum((1 - X), first, X, 1.0) / 3.

tau_sum(Z, Z, _X, _Y) ->
    Z;
tau_sum(Z, _Zp, X, Y) ->
    X1 = math:sqrt(X),
    Y1 = Y * 0.5,
    Z1 = Z - (math:pow(1 - X1, 2) * Y1),
    tau_sum(Z1, Z, X1, Y1).

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
