%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(pqueue_priority0).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data8() ->
    priority_queue:new().

data9() ->
    pqueue:new().

data10() ->
    pqueue2:new().

data11() ->
    pqueue3:new().

priority_queue_set_0(Queue, _, Value) ->
    priority_queue:in(Value, Queue).

pqueue_set_0(Queue, _, Value) ->
    pqueue:in(Value, Queue).

pqueue2_set_0(Queue, _, Value) ->
    pqueue2:in(Value, Queue).

pqueue3_set_0(Queue, _, Value) ->
    pqueue3:in(Value, Queue).

priority_queue_get(Queue) ->
    {{value, _}, NewQueue} = priority_queue:out(Queue),
    NewQueue.
    
pqueue_get(Queue) ->
    {{value, _}, NewQueue} = pqueue:out(Queue),
    NewQueue.
    
pqueue2_get(Queue) ->
    {{value, _}, NewQueue} = pqueue2:out(Queue),
    NewQueue.

pqueue3_get(Queue) ->
    {{value, _}, NewQueue} = pqueue3:out(Queue),
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
    {S8, D8} = timer:tc(run_priority_queue, set, [fun priority_queue_set_0/3, data8(), N]),
    {G8, _} = timer:tc(run_priority_queue, get, [fun priority_queue_get/1, D8, N]),
    {S9, D9} = timer:tc(run_priority_queue, set, [fun pqueue_set_0/3, data9(), N]),
    {G9, _} = timer:tc(run_priority_queue, get, [fun pqueue_get/1, D9, N]),
    {S10, D10} = timer:tc(run_priority_queue, set, [fun pqueue2_set_0/3, data10(), N]),
    {G10, _} = timer:tc(run_priority_queue, get, [fun pqueue2_get/1, D10, N]),
    {S11, D11} = timer:tc(run_priority_queue, set, [fun pqueue3_set_0/3, data11(), N]),
    {G11, _} = timer:tc(run_priority_queue, get, [fun pqueue3_get/1, D11, N]),
    [
        #result{name = "priority_queue",       get =  G8, set =  S8},
        #result{name = "pqueue",               get =  G9, set =  S9},
        #result{name = "pqueue2",              get =  G10, set =  S10},
        #result{name = "pqueue3",              get =  G11, set =  S11}
    ].

