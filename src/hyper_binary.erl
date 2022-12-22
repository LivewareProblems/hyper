%% @doc: Registers stored in one large binary
%%
%% This backend uses one plain Erlang binary to store registers. The
%% cost of rebuilding the binary is amortized by keeping a buffer of
%% inserts to perform in the future.

-module(hyper_binary).

-behaviour(hyper_register).

%%-compile(native).

-export([
    new/1,
    set/3,
    compact/1,
    max_merge/1, max_merge/2,
    reduce_precision/2,
    bytes/1,
    register_sum/1,
    register_histogram/1,
    zero_count/1,
    encode_registers/1,
    decode_registers/2,
    empty_binary/1,
    max_registers/1,
    precision/1
]).

-define(VALUE_SIZE, 6).
-define(MERGE_THRESHOLD, 0.05).

-type precision() :: 4..16.

-record(buffer, {buf, buf_size, p :: precision(), convert_threshold}).
-record(dense, {b, buf, buf_size, p :: precision(), merge_threshold}).

new(P) ->
    new_buffer(P).

new_buffer(P) ->
    M = hyper_utils:m(P),
    % 5 words for each entry
    ConvertThreshold = M div (5 * 8),
    #buffer{
        buf = [],
        buf_size = 0,
        p = P,
        convert_threshold = ConvertThreshold
    }.

new_dense(P) ->
    M = hyper_utils:m(P),
    T = max(trunc(M * ?MERGE_THRESHOLD), 16),
    #dense{
        b = empty_binary(M),
        buf = [],
        buf_size = 0,
        p = P,
        merge_threshold = T
    }.

set(Index, Value, #buffer{buf = [{Index, OldValue} | Rest]} = Buffer) ->
    NewVal = hyper_utils:run_of_zeroes(Value),
    Buffer#buffer{buf = [{Index, max(NewVal, OldValue)} | Rest]};
set(Index, Value, #buffer{buf = Buf, buf_size = BufSize} = Buffer) ->
    NewVal = hyper_utils:run_of_zeroes(Value),
    NewBuffer = Buffer#buffer{buf = [{Index, NewVal} | Buf], buf_size = BufSize + 1},
    case NewBuffer#buffer.buf_size < NewBuffer#buffer.convert_threshold of
        true ->
            NewBuffer;
        false ->
            buffer2dense(NewBuffer)
    end;
set(Index, Value, #dense{buf = Buf, buf_size = BufSize} = Dense) ->
    LeftOffset = Index * ?VALUE_SIZE,
    <<_:LeftOffset/bitstring, R:?VALUE_SIZE/integer, _/bitstring>> = Dense#dense.b,

    NewVal = hyper_utils:run_of_zeroes(Value),

    if
        R < NewVal ->
            New = Dense#dense{buf = [{Index, NewVal} | Buf], buf_size = BufSize + 1},
            case New#dense.buf_size < Dense#dense.merge_threshold of
                true ->
                    New;
                false ->
                    compact(New)
            end;
        true ->
            Dense
    end.

compact(#buffer{buf_size = BufSize, convert_threshold = ConvertThreshold} = Buffer) ->
    case BufSize < ConvertThreshold of
        true ->
            Buffer;
        false ->
            buffer2dense(Buffer)
    end;
compact(#dense{b = B, buf = Buf} = Dense) ->
    NewB = merge_buf(B, max_registers(Buf)),
    Dense#dense{
        b = NewB,
        buf = [],
        buf_size = 0
    }.

max_merge([First | Rest]) ->
    lists:foldl(fun max_merge/2, First, Rest).

max_merge(
    #dense{
        b = SmallB,
        buf = SmallBuf,
        buf_size = SmallBufSize
    },
    #dense{
        b = BigB,
        buf = BigBuf,
        buf_size = BigBufSize
    } =
        Big
) ->
    case SmallBufSize + BigBufSize < Big#dense.merge_threshold of
        true ->
            Merged = do_merge(SmallB, BigB, <<>>),
            Big#dense{
                b = Merged,
                buf = SmallBuf ++ BigBuf,
                buf_size = SmallBufSize + BigBufSize
            };
        false ->
            BigWithBuf = merge_buf(BigB, max_registers(SmallBuf ++ BigBuf)),
            Merged = do_merge(SmallB, BigWithBuf, <<>>),
            Big#dense{
                b = Merged,
                buf = [],
                buf_size = 0
            }
    end;
max_merge(
    #buffer{buf = Buf, buf_size = BufferSize},
    #dense{buf = DenseBuf, buf_size = DenseSize} = Dense
) ->
    case BufferSize + DenseSize < Dense#dense.merge_threshold of
        true ->
            Dense#dense{buf = Buf ++ DenseBuf, buf_size = BufferSize + DenseSize};
        false ->
            Merged = max_registers(DenseBuf ++ Buf),
            Dense#dense{buf = Merged, buf_size = length(Merged)}
    end;
max_merge(#dense{} = Dense, #buffer{} = Buffer) ->
    max_merge(Buffer, Dense);
max_merge(
    #buffer{buf = LeftBuf, buf_size = LeftBufSize},
    #buffer{buf = RightBuf, buf_size = RightBufSize} = Right
) ->
    case LeftBufSize + RightBufSize < Right#buffer.convert_threshold of
        true ->
            Right#buffer{buf = LeftBuf ++ RightBuf, buf_size = LeftBufSize + RightBufSize};
        false ->
            Merged = max_registers(LeftBuf ++ RightBuf),
            NewRight = Right#buffer{buf = Merged, buf_size = length(Merged)},
            case NewRight#buffer.buf_size < NewRight#buffer.convert_threshold of
                true ->
                    NewRight;
                false ->
                    buffer2dense(NewRight)
            end
    end.

reduce_precision(NewP, #dense{p = OldP} = Dense) ->
    ChangeP = OldP - NewP,
    Buf = register_fold(ChangeP, Dense),
    Empty = new_dense(NewP),
    compact(Empty#dense{buf = Buf});
reduce_precision(NewP, #buffer{p = OldP} = Buffer) ->
    ChangeP = OldP - NewP,
    Buf = register_fold(ChangeP, Buffer),
    Empty = new_dense(NewP),
    compact(Empty#dense{buf = Buf}).

%% fold an HLL to a lower number of registers `NewM` by projecting the values
%% onto their new index, see
%% http://research.neustar.biz/2012/09/12/set-operations-on-hlls-of-different-sizes/
%% NOTE: this function does not perform the max_registers step
register_fold(ChangeP, B) ->
    ChangeM = hyper_utils:m(ChangeP),
    element(
        3,
        fold(
            fun
                (I, V, {_Index, CurrentList, Acc}) when CurrentList == [] ->
                    {I, [V], Acc};
                (I, V, {Index, CurrentList, Acc}) when I - Index < ChangeM ->
                    {Index, [V | CurrentList], Acc};
                (I, V, {_Index, CurrentList, Acc}) ->
                    {I, [V], [
                        {I bsr ChangeP, hyper_utils:changeV(lists:reverse(CurrentList), ChangeP)}
                        | Acc
                    ]}
            end,
            {0, [], []},
            B
        )
    ).

register_sum(B) ->
    fold(
        fun
            (_, 0, Acc) ->
                Acc + 1.0;
            (_, 1, Acc) ->
                Acc + 0.5;
            (_, 2, Acc) ->
                Acc + 0.25;
            (_, 3, Acc) ->
                Acc + 0.125;
            (_, 4, Acc) ->
                Acc + 0.0625;
            (_, 5, Acc) ->
                Acc + 0.03125;
            (_, 6, Acc) ->
                Acc + 0.015625;
            (_, 7, Acc) ->
                Acc + 0.0078125;
            (_, 8, Acc) ->
                Acc + 0.00390625;
            (_, 9, Acc) ->
                Acc + 0.001953125;
            (_, V, Acc) ->
                Acc + math:pow(2, -V)
        end,
        0,
        B
    ).

register_histogram(#buffer{p = P} = B) ->
    register_histogram(B, P);
register_histogram(#dense{p = P} = B) ->
    register_histogram(B, P).

register_histogram(B, P) ->
    % todo use from_keys once we are at otp 26
    fold(
        fun(_, Value, Acc) -> maps:update_with(Value, fun(V) -> V + 1 end, 0, Acc) end,
        maps:from_list(
            lists:map(fun(I) -> {I, 0} end, lists:seq(0, 65 - P))
        ),
        B
    ).

zero_count(B) ->
    fold(
        fun
            (_, 0, Acc) ->
                Acc + 1;
            (_, _, Acc) ->
                Acc
        end,
        0,
        B
    ).

encode_registers(#buffer{} = Buffer) ->
    encode_registers(buffer2dense(Buffer));
encode_registers(#dense{b = B}) ->
    <<<<I:8/integer>> || <<I:?VALUE_SIZE/integer>> <= B>>.

decode_registers(AllBytes, P) ->
    M = hyper_utils:m(P),
    Bytes =
        case AllBytes of
            <<B:M/binary>> ->
                B;
            <<B:M/binary, 0>> ->
                B
        end,
    Dense = new_dense(P),
    Dense#dense{b = <<<<I:?VALUE_SIZE/integer>> || <<I:8>> <= Bytes>>}.

bytes(#dense{b = B}) ->
    erlang:byte_size(B);
bytes(#buffer{} = Buffer) ->
    erts_debug:flat_size(Buffer) * 8.

precision(#dense{p = P}) ->
    P;
precision(#buffer{p = P}) ->
    P.

%%
%% INTERNALS
%%

empty_binary(M) ->
    list_to_bitstring([<<0:?VALUE_SIZE/integer>> || _ <- lists:seq(0, M - 1)]).

max_registers(Buf) ->
    lists:keysort(
        1,
        maps:to_list(
            lists:foldl(
                fun({I, V}, Acc) ->
                    case maps:find(I, Acc) of
                        {ok, R} when R >= V -> Acc;
                        _ -> maps:put(I, V, Acc)
                    end
                end,
                #{},
                Buf
            )
        )
    ).

buffer2dense(#buffer{buf = Buf, p = P}) ->
    Dense = new_dense(P),
    Merged = merge_buf(Dense#dense.b, max_registers(Buf)),
    Dense#dense{b = Merged}.

do_merge(<<>>, <<>>, Acc) ->
    Acc;
do_merge(
    <<Left:?VALUE_SIZE/integer, SmallRest/bitstring>>,
    <<Right:?VALUE_SIZE/integer, BigRest/bitstring>>,
    Acc
) ->
    do_merge(SmallRest, BigRest, <<Acc/bits, (max(Left, Right)):?VALUE_SIZE>>).

fold(F, Acc, #buffer{} = Buffer) ->
    % TODO fix this to not need to move to a dense, it is not technically neeeded
    % We can just fold on the list i think
    fold(F, Acc, buffer2dense(Buffer));
fold(F, Acc, #dense{b = B, buf = Buf}) ->
    do_fold(F, Acc, merge_buf(B, max_registers(Buf)), 0).

do_fold(_, Acc, <<>>, _) ->
    Acc;
do_fold(F, Acc, <<Value:?VALUE_SIZE/integer, Rest/bitstring>>, Index) ->
    do_fold(F, F(Index, Value, Acc), Rest, Index + 1).

merge_buf(B, L) ->
    merge_buf(B, L, -1, <<>>).

merge_buf(B, [], _PrevIndex, Acc) ->
    <<Acc/bitstring, B/bitstring>>;
merge_buf(B, [{Index, Value} | Rest], PrevIndex, Acc) ->
    I = (Index - PrevIndex - 1) * ?VALUE_SIZE,
    case B of
        <<Left:I/bitstring, OldValue:?VALUE_SIZE/integer, Right/bitstring>> ->
            case OldValue < Value of
                true ->
                    NewAcc = <<Acc/bitstring, Left/bitstring, Value:?VALUE_SIZE/integer>>,
                    merge_buf(Right, Rest, Index, NewAcc);
                false ->
                    NewAcc = <<Acc/bitstring, Left/bitstring, OldValue:?VALUE_SIZE/integer>>,
                    merge_buf(Right, Rest, Index, NewAcc)
            end;
        <<Left:I/bitstring>> ->
            <<Acc/bitstring, Left/bitstring, Value:?VALUE_SIZE/integer>>
    end.
