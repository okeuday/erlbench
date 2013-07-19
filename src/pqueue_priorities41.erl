%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(pqueue_priorities41).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

data1() ->
    priority_queue:new().

data2() ->
    pqueue:new().

data3() ->
    pqueue2:new().

data4() ->
    pqueue3:new([{priorities, 41}]).

data5() ->
    pqueue4:new().

priority_queue_set_41(Queue, P, Value) ->
    priority_queue:in(Value, (P rem 41) - 20, Queue).

pqueue_set(Queue, P, Value) ->
    pqueue:in(Value, (P rem 41) - 20, Queue).

pqueue2_set_41(Queue, P, Value) ->
    pqueue2:in(Value, (P rem 41) - 20, Queue).

pqueue3_set_41(Queue, P, Value) ->
    pqueue3:in(Value, (P rem 41) - 20, Queue).

pqueue4_set_41(Queue, P, Value) ->
    pqueue4:in(Value, (P rem 41) - 20, Queue).

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

pqueue4_get(Queue) ->
    {{value, _}, NewQueue} = pqueue4:out(Queue),
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
    {S1, D1} = timer:tc(?MODULE, set, [fun priority_queue_set_41/3, data1(), N]),
    {G1, _} = timer:tc(?MODULE, get, [fun priority_queue_get/1, D1, N]),
    {S2, D2} = timer:tc(?MODULE, set, [fun pqueue_set/3, data2(), N]),
    {G2, _} = timer:tc(?MODULE, get, [fun pqueue_get/1, D2, N]),
    {S3, D3} = timer:tc(?MODULE, set, [fun pqueue2_set_41/3, data3(), N]),
    {G3, _} = timer:tc(?MODULE, get, [fun pqueue2_get/1, D3, N]),
    {S4, D4} = timer:tc(?MODULE, set, [fun pqueue3_set_41/3, data4(), N]),
    {G4, _} = timer:tc(?MODULE, get, [fun pqueue3_get/1, D4, N]),
    {S5, D5} = timer:tc(?MODULE, set, [fun pqueue4_set_41/3, data5(), N]),
    {G5, _} = timer:tc(?MODULE, get, [fun pqueue4_get/1, D5, N]),
    [
        #result{name = "41 priority_queue",       get =  G1, set =  S1},
        #result{name = "41 pqueue",               get =  G2, set =  S2},
        #result{name = "41 pqueue2",              get =  G3, set =  S3},
        #result{name = "41 pqueue3",              get =  G4, set =  S4},
        #result{name = "41 pqueue4",              get =  G5, set =  S5}
    ].

