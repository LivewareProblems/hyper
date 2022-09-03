-module(prop_hyper).

-export([prop_set/0]).

-include_lib("proper/include/proper.hrl").
% -include_lib("common_test/include/ct_property_test.hrl").

%%
%% PROPERTIES
%%

backends() ->
    [hyper_binary].

gen_values() ->
    ?SIZED(Size, gen_values(Size)).

gen_values(0) ->
    [<<(rand:uniform(100000000000000)):64/integer>>];
gen_values(Size) ->
    [<<(rand:uniform(100000000000000)):64/integer>> | gen_values(Size - 1)].

%%gen_values(0) ->
%%    [non_empty(binary())];
%%gen_values(Size) ->
%%    [non_empty(binary()) | gen_values(Size-1)].

gen_getset(P) ->
    ?SIZED(Size, gen_getset(Size, P)).

gen_getset(0, _P) ->
    [];
gen_getset(Size, P) ->
    M = trunc(math:pow(2, P)),
    ?LET(
        {I, V},
        {choose(0, M - 1), choose(1, 6)},
        ?LET(X, trunc(math:pow(2, 64 - V - P + 1)) - 1, [
            {I, <<0:V, X:(64 - V - P)>>} | gen_getset(Size - 1, P)
        ])
    ).

prop_set() ->
    ?FORALL(
        {Mod, P},
        {oneof(backends()), choose(4, 16)},
        ?FORALL(
            Values,
            gen_getset(P),
            begin
                R = lists:foldl(
                    fun({Index, BinaryValue}, Register) ->
                        Mod:set(Index, BinaryValue, Register)
                    end,
                    Mod:new(P),
                    Values
                ),
                Max = lists:foldl(
                    fun({I, V}, Acc) ->
                        RoZV = hyper_utils:run_of_zeroes(V),
                        case dict:find(I, Acc) of
                            {ok, RoZOtherV} when RoZOtherV >= RoZV ->
                                Acc;
                            _ ->
                                dict:store(I, RoZV, Acc)
                        end
                    end,
                    dict:new(),
                    Values
                ),
                Expected = lists:map(
                    fun(I) ->
                        case dict:find(I, Max) of
                            {ok, V} ->
                                <<V:8/integer>>;
                            error ->
                                <<0>>
                        end
                    end,
                    lists:seq(0, trunc(math:pow(2, P)) - 1)
                ),
                case Mod:encode_registers(Mod:compact(R)) =:= iolist_to_binary(Expected) of
                    true ->
                        true;
                    false ->
                        %% error_logger:info_msg("values~n~p~n"
                        %%                       "encoded~n~p~n"
                        %%                       "expected~n~p~n",
                        %%                       [Values,
                        %%                        Mod:encode_registers(R),
                        %%                        iolist_to_binary(Expected)]),
                        false
                end
            end
        )
    ).

prop_serialize() ->
    ?FORALL(
        {LeftMod, RightMod, P, Values},
        {oneof(backends()), oneof(backends()), choose(4, 16), gen_values()},
        begin
            Left = hyper:compact(hyper:insert_many(Values, hyper:new(P, LeftMod))),
            Right = hyper:compact(hyper:insert_many(Values, hyper:new(P, RightMod))),
            hyper:to_json(Left) =:= hyper:to_json(Right)
        end
    ).

prop_union_binary() ->
    ?FORALL(
        {P, NumFilters, Values},
        {choose(4, 16), choose(2, 5), gen_values()},
        begin
            Filters = lists:map(
                fun(Vs) ->
                    hyper:insert_many(Vs, hyper:new(P, hyper_binary))
                end,
                partition(NumFilters, Values)
            ),
            Filter = hyper:insert_many(Values, hyper:new(P, hyper_binary)),
            Union = hyper:union(Filters),
            hyper:card(Filter) =:= hyper:card(Union)
        end
    ).

%%
%% HELPERS
%%

%% Lifted from stdlib2, https://github.com/cannedprimates/stdlib2
partition(N, Xs) when is_integer(N), N > 0, is_list(Xs) ->
    Len = length(Xs),
    case {Len > 0, Len > N} of
        {true, true} ->
            [take(N, Xs) | partition(N, drop(N, Xs))];
        {true, false} ->
            [Xs];
        {false, false} ->
            []
    end.

take(N, _) when N =< 0 ->
    [];
take(_, []) ->
    [];
take(N, [X | Xs]) ->
    [X | take(N - 1, Xs)].

drop(N, Xs) when N =< 0 ->
    Xs;
drop(_, []) ->
    [];
drop(N, [_ | Xs]) ->
    drop(N - 1, Xs).
