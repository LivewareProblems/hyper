-module(hyper_binary_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [merge_test, serialize_test, max_registers_test].

-define(VALUE_SIZE, 6).

%%
%% TESTS
%%

merge_test(_Config) ->
    P = 4,
    M = hyper_utils:m(P),
    Tmp1 = [{1, 1}, {3, 3}, {9, 3}, {15, 15}],
    Tmp2 = [{3, 5}, {9, 2}, {10, 5}],

    {buffer, [], 0, T, _} = hyper_binary:new(P),

    {dense, Compact, [], 0, _P, _} = hyper_binary:compact(
        {dense, hyper_binary:empty_binary(M), Tmp1, length(Tmp1), P, T}
    ),

    {dense, Compact2, [], 0, _P1, _} = hyper_binary:compact(
        {dense, Compact, Tmp2, length(Tmp2), P, T}
    ),

    Expected = [0, 1, 0, 5, 0, 0, 0, 0, 0, 3, 5, 0, 0, 0, 0, 15],
    Ints = [I || <<I:?VALUE_SIZE/integer>> <= Compact2],

    ?assertEqual(Expected, Ints).

serialize_test(_Config) ->
    H = hyper_binary:compact(
        lists:foldl(
            fun(I, Acc) -> hyper_binary:set(I, <<0:I/integer, 1:1/integer>>, Acc) end,
            hyper_binary:new(4),
            lists:seq(0, 15)
        )
    ),

    ?assertEqual(
        <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
        hyper_binary:encode_registers(H)
    ),
    ?assertEqual(H, hyper_binary:decode_registers(hyper_binary:encode_registers(H), 4)).

max_registers_test(_Config) ->
    ?assertEqual([{3, 3}], hyper_binary:max_registers([{3, 1}, {3, 2}, {3, 3}])).
