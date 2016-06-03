%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(math_speed).

-export([test/1, run/2]).

-include("erlbench.hrl").

test_ceil_1() ->
    X = 1.5,
    T = trunc(X),
    if
       X > T ->
           T + 1;
       true ->
           T
    end.

test_ceil_2() ->
    X = 1.5,
    trunc(trunc(X)).

run(1, F) ->
    F();
run(N, F) ->
    F(),
    run(N - 1, F).

test(N) ->
    {Test1, _} = timer:tc(?MODULE, run, [N, fun test_ceil_1/0]),
    {Test2, _} = timer:tc(?MODULE, run, [N, fun test_ceil_2/0]),

    %% results
    [
        #result{name = "ceil_1",                     get =  Test1},
        #result{name = "ceil_2",                     get =  Test2}
    ].

