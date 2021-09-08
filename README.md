# HyperLogLog for Erlang

![Hex.pm](https://img.shields.io/hexpm/v/hyper?style=flat-square)
![Hex.pm](https://img.shields.io/hexpm/l/hyper?style=flat-square)

This is an implementation of the HyperLogLog algorithm in
Erlang. Using HyperLogLog you can estimate the cardinality of very
large data sets using constant memory. The relative error is `1.04 *
sqrt(2^P)`. When creating a new HyperLogLog filter, you provide the
precision P, allowing you to trade memory for accuracy. The union of
two filters is lossless.

In practice this allows you to build efficient analytics systems. For
example, you can create a new filter in each mapper and feed it a
portion of your dataset while the reducers simply union together all
filters they receive. The filter you end up with is exactly the same
filter as if you would sequentially insert all data into a single
filter.

In addition to the base algorithm, we have implemented the new estimator as
based on Mean Limit as described this great [paper by Otmar Ertl][].
This new estimator greatly improves the estimates for lower cardinalities while
using a single estimator for the whole range of cardinalities.

## TODO

- [x] Use rebar3
- [x] Work on OTP 23
- [x] Fix the estimator
- [x] Fix `reduce_precision`
- [x] add `reduce_precision` for array, allowing unions
- [ ] Better document the main module
- [ ] Move documentation to ExDoc
- [ ] Delete dead code
- [ ] Rework test suite to be nice to modify
- [ ] Rework Intersection using this [paper by Otmar Ertl][]
- [ ] Redo benchmarks

## Usage

```erlang
1> hyper:insert(<<"foobar">>, hyper:insert(<<"quux">>, hyper:new(4))).
{hyper,4,
       {hyper_binary,{dense,<<0,0,0,0,0,0,0,0,64,0,0,0>>,
                            [{8,1}],
                            1,16}}}

2> hyper:card(v(-1)).
2.136502281992361
```

The errors introduced by estimations can be seen in this example:

```erlang
3> rand:seed(exsss, {1, 2, 3}).
{#{bits => 58,jump => #Fun<rand.3.47293030>,
   next => #Fun<rand.0.47293030>,type => exsss,
   uniform => #Fun<rand.1.47293030>,
   uniform_n => #Fun<rand.2.47293030>},
 [117085240290607817|199386643319833935]}
4> Run = fun (P, Card) -> hyper:card(lists:foldl(fun (_, H) -> Int = rand:uniform(10000000000000), hyper:insert(<<Int:64/integer>>, H) end, hyper:new(P), lists:seq(1, Card))) end.
#Fun<erl_eval.12.80484245>
5> Run(12, 10_000).
10038.192365345985
6> Run(14, 10_000).
9967.916262642864
7> Run(16, 10_000).
9972.832893293473
```

A filter can be persisted and read later. The serialized struct is formatted for usage with jiffy:

```erlang
8> Filter = hyper:insert(<<"foo">>, hyper:new(4)).
{hyper,4,
       {hyper_binary,{dense,<<4,0,0,0,0,0,0,0,0,0,0,0>>,[],0,16}}}
9> Filter =:= hyper:from_json(hyper:to_json(Filter)).
true
```

You can select a different backend. See below for a description of why
you might want to do so. They serialize in exactly the same way, but
can't be mixed in memory.

```erlang
1> Gb = hyper:insert(<<"foo">>, hyper:new(4, hyper_array)).
{hyper,4, {hyper_array,{array,16,0,0, {{1,0,0,0,0,0,0,0,0,0},10,10,10,10,10,10,10,10,10,10}}}}
2> B = hyper:insert(<<"foo">>, hyper:new(4, hyper_binary)).
{hyper,4,
       {hyper_binary,{dense,<<4,0,0,0,0,0,0,0,0,0,0,0>>,[],0,4,16}}}
3> hyper:to_json(Gb) =:= hyper:to_json(B).
true
4> hyper:union(Gb, B).
** exception error: no case clause matching [{4,hyper_binary},{4,hyper_array}]
     in function  hyper:union/1 (src/hyper.erl, line 65)
```

## Is it any good?

No idea ! I do not know anyone that uses it extensively, but it is relatively
well tested. As far as i can tell, it is the only FOSS implementation that does
precision reduction properly !

## Backends

Effort has been spent on implementing different backends in the
pursuit of finding the right performance trade-off. The estimate will
always be the same, regardless of backend. A simple performance
comparison can be seen by running `make perf_report`, see below for
the results from an i7-3770 at 3.4 GHz. Fill rate refers to how many
registers has a value other than 0.

- `hyper_binary`: Fixed memory usage (6 bits * 2^P), fastest on insert,
  union, cardinality and serialization. Best default choice.
- `hyper_array`: Cardinality estimation is constant, but slower than
  hyper_gb for low fill rates. Uses much more memory at lower fill
  rates, but stays constant from 25% and upwards.

You can also implement your own backend. In `hyper_test` theres a
bunch of tests run for all backends, including some PropEr tests. The
test suite will ensure your backend gives correct estimates and
correctly encodes/decodes the serialized filters.

```bash
$ make perf_report
...
```

## Fork

This is a fork of the original Hyper library by GameAnalytics. It was not
maintained anymore.

The main difference are a move to the `rand` module for tests and to `rebar3`
as a build tool, in order to support OTP 23+.

The `carray` backend was dropped, as it was never moved outside of experimental
status and could not be serialised for a distributed use. Some backends using
NIF may come back in the future.

The bisect implementation was dropped too. Its use case was limited and it
forced a dependency on a library that was not maintained either.

The gb backend was dropped for the time being too.

The estimator was rebuilt following this [paper by Otmar Ertl][], as it was
brokenfor any precision not 14. This should also provide better estimation
across the board for cardinality.

The `reduce_precision` function has been rebuilt properly, as it was quite
simply wrong. This fixed a lot of bugs for unions.

[paper by Otmar Ertl]: https://arxiv.org/abs/1706.07290
