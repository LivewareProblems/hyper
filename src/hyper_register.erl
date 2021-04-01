%% @doc: If you wish to implement your own backend for storing
%% registers, your module needs to implement these interfaces. The
%% backend modules have quite a lot of responsibility (detailed below)
%% to allow for backend-specific optimizations.
-module(hyper_register).

-callback set(Index :: integer(),
              Value :: integer(),
              hyper:registers()) ->
    hyper:registers().
%% %% @doc: Set the register to the given value, *only* if the value
%% already stored is lower than the new value. The backend needs to
%% ensure the register value is only allowed to increase.

-callback new(P :: hyper:precision()) ->
    hyper:registers().
%% %% @doc: Creates a new instance of the backend. The return value of
%% this function will be passed to all functions in this module.

-callback compact(hyper:registers()) ->
    hyper:registers().
%% %% @doc: Compact is always called before any attempt at reading (sum,
%% zero count, etc) or merging. It is intended to give backends that
%% buffer the writes a chance to flush the buffer before the registers
%% are needed.

-callback max_merge([hyper:registers()]) ->
    hyper:registers().
%% %% @doc: Merge any number of registers, used to calculate the
%% union. For two register values at the same index, the max value
%% must be in the resulting register.

-callback max_merge(hyper:registers(),
                    hyper:registers()) ->
    hyper:registers().
%% %% @doc: Same as max_merge/1 but used when we know only two filters
%% are merged.

-callback reduce_precision(hyper:precision(),
                           hyper:registers()) ->
    hyper:registers().
%% %% @doc: Reduce the precision of the registers. Used for mixed-precision
%% union by first reducing the precision to the lowest of all filters.

-callback register_sum(hyper:registers()) ->
    float().
%% %% @doc: Sum of 2^-R where R is the value in each register.

-callback zero_count(hyper:registers()) ->
    integer().
%% %% @doc: Count of registers set to 0.

-callback encode_registers(hyper:registers()) ->
    binary().
%% %% @doc: Encode and decode are called to convert the in-memory
%% representation of the backend to the serialized format. Must return
%% one binary where each register is encoded as an 8-bit integer.

-callback decode_registers(binary(), hyper:precision()) ->
    hyper:registers().

-callback bytes(hyper:registers()) ->
    integer().
%% %% @doc: Size in bytes used to represent the registers in memory.