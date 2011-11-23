%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(pqueue_priorities2).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data12() ->
    priority_queue:new().

data13() ->
    pqueue:new().

data14() ->
    pqueue2:new().

data15() ->
    pqueue3:new().

priority_queue_set_2(Queue, P, Value) ->
    priority_queue:in(Value, (P rem 2) - 1, Queue).

pqueue_set_2(Queue, P, Value) ->
    pqueue:in(Value, (P rem 2) - 1, Queue).

pqueue2_set_2(Queue, P, Value) ->
    pqueue2:in(Value, (P rem 2) - 1, Queue).

pqueue3_set_2(Queue, P, Value) ->
    pqueue3:in(Value, (P rem 2) - 1, Queue).

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
    {S12, D12} = timer:tc(run_priority_queue, set, [fun priority_queue_set_2/3, data12(), N]),
    {G12, _} = timer:tc(run_priority_queue, get, [fun priority_queue_get/1, D12, N]),
    {S13, D13} = timer:tc(run_priority_queue, set, [fun pqueue_set_2/3, data13(), N]),
    {G13, _} = timer:tc(run_priority_queue, get, [fun pqueue_get/1, D13, N]),
    {S14, D14} = timer:tc(run_priority_queue, set, [fun pqueue2_set_2/3, data14(), N]),
    {G14, _} = timer:tc(run_priority_queue, get, [fun pqueue2_get/1, D14, N]),
    {S15, D15} = timer:tc(run_priority_queue, set, [fun pqueue3_set_2/3, data15(), N]),
    {G15, _} = timer:tc(run_priority_queue, get, [fun pqueue3_get/1, D15, N]),
    [
        #result{name = "2x priority_queue",        get =  G12, set =  S12},
        #result{name = "2x pqueue",                get =  G13, set =  S13},
        #result{name = "2x pqueue2",               get =  G14, set =  S14},
        #result{name = "2x pqueue3",               get =  G15, set =  S15}
    ].

