%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

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
    {S1, D1} = timer:tc(queue_in_out, set, [fun queue_set/2, data1(N), N]),
    {G1, _} = timer:tc(queue_in_out, get, [fun queue_get/1, D1, N]),
    %% lqueue
    {S2, D2} = timer:tc(queue_in_out, set, [fun lqueue_set/2, data2(N), N]),
    {G2, _} = timer:tc(queue_in_out, get, [fun lqueue_get/1, D2, N]),
    %% results
    [
        #result{name = "queue",               get =  G1, set =  S1},
        #result{name = "lqueue",              get =  G2, set =  S2}
    ].

