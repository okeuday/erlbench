%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(run_priority_queue).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data1() ->
    priority_queue:new().

data2() ->
    pqueue:new().

priority_queue_set(Queue, P, Value) ->
    priority_queue:in(Value, (P rem 41) - 20, Queue).

pqueue_set(Queue, P, Value) ->
    pqueue:in(Value, (P rem 41) - 20, Queue).

priority_queue_get(Queue) ->
    {{value, _}, NewQueue} = priority_queue:out(Queue),
    NewQueue.
    
pqueue_get(Queue) ->
    {{value, _}, NewQueue} = pqueue:out(Queue),
    NewQueue.
    
get(_, _, 0) ->
    ok;

get(Fun, Data, N) ->
    get(Fun, Fun(Data), N - 1).

set(_, Data, 0) ->
    Data;

set(Fun, Data, N) ->
    Data1 = Fun(Data, N, N),
    set(Fun, Data1, N - 1).

test() ->
    test(10000).

test(N) ->
    {S1, D1} = timer:tc(run_priority_queue, set, [fun priority_queue_set/3, data1(), N]),
    {G1, _} = timer:tc(run_priority_queue, get, [fun priority_queue_get/1, D1, N]),
    {S2, D2} = timer:tc(run_priority_queue, set, [fun pqueue_set/3, data2(), N]),
    {G2, _} = timer:tc(run_priority_queue, get, [fun pqueue_get/1, D2, N]),
    [
        #result{name = "priority_queue",       get =  G1, set =  S1},
        #result{name = "pqueue",               get =  G2, set =  S2}
    ].

