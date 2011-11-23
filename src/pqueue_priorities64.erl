%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(pqueue_priorities64).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data5() ->
    priority_queue:new().

data6() ->
    pqueue2:new().

data7() ->
    pqueue3:new([{priorities, 64}]).

priority_queue_set_64(Queue, P, Value) ->
    priority_queue:in(Value, (P rem 64) - 32, Queue).

pqueue2_set_64(Queue, P, Value) ->
    pqueue2:in(Value, (P rem 64) - 32, Queue).

pqueue3_set_64(Queue, P, Value) ->
    pqueue3:in(Value, (P rem 64) - 32, Queue).

priority_queue_get(Queue) ->
    {{value, _}, NewQueue} = priority_queue:out(Queue),
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
    {S5, D5} = timer:tc(run_priority_queue, set, [fun priority_queue_set_64/3, data5(), N]),
    {G5, _} = timer:tc(run_priority_queue, get, [fun priority_queue_get/1, D5, N]),
    {S6, D6} = timer:tc(run_priority_queue, set, [fun pqueue2_set_64/3, data6(), N]),
    {G6, _} = timer:tc(run_priority_queue, get, [fun pqueue2_get/1, D6, N]),
    {S7, D7} = timer:tc(run_priority_queue, set, [fun pqueue3_set_64/3, data7(), N]),
    {G7, _} = timer:tc(run_priority_queue, get, [fun pqueue3_get/1, D7, N]),
    [
        #result{name = "64 priority_queue",       get =  G5, set =  S5},
        #result{name = "64 pqueue2",              get =  G6, set =  S6},
        #result{name = "64 pqueue3",              get =  G7, set =  S7}
    ].

