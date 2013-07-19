%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(queue_in_out).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data1(_) ->
    queue:new().

data2(_) ->
    lqueue:new().

queue_set(Value, Q) ->
    queue:in(Value, Q).

lqueue_set(Value, Q) ->
    lqueue:in(Value, Q).

queue_get(Q) ->
    queue:out(Q).

lqueue_get(Q) ->
    lqueue:out(Q).

get(_, _, 0) ->
    ok;

get(Fun, Data, N) ->
    true = N == Fun(Data),
    get(Fun, Data, N - 1).

set(_, Data, 0) ->
    Data;

set(Fun, Data, N) ->
    Data1 = Fun(N, Data),
    set(Fun, Data1, N - 1).

test() ->
    test(10000).

test(N) ->
    %% queue
    {S1, D1} = timer:tc(?MODULE, set, [fun queue_set/2, data1(N), N]),
    {G1, _} = timer:tc(?MODULE, get, [fun queue_get/1, D1, N]),
    %% lqueue
    {S2, D2} = timer:tc(?MODULE, set, [fun lqueue_set/2, data2(N), N]),
    {G2, _} = timer:tc(?MODULE, get, [fun lqueue_get/1, D2, N]),
    %% results
    [
        #result{name = "queue",               get =  G1, set =  S1},
        #result{name = "lqueue",              get =  G2, set =  S2}
    ].

