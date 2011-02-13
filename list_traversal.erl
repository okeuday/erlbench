%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(list_traversal).

-export([test/0, test/1,
         traverse_map/1,
         traverse_reverse/1,
         traverse_append/1,
         traverse_queue/1,
         traverse_as_queue/1]).

-include("erlbench.hrl").


create(N) ->
    lists:seq(1, N).

traverse_map(L) ->
    lists:map(fun(I) -> I + 1 end, L).

traverse_reverse(L) ->
    traverse_reverse([], lists:reverse(L)).
    
traverse_reverse(L, [H | T]) ->
    traverse_reverse([H | L], T).

traverse_append(L) ->
    traverse_append([], L).

traverse_append(L, [H | T]) ->
    traverse_append(L ++ [H], T).

traverse_queue(L) ->
    traverse_queue(queue:new(), queue:from_list(L)).

traverse_queue(Qnew, Qold1) ->
    case queue:out(Qold1) of
        {{value, Item}, Qold2} ->
            traverse_queue(queue:in(Item, Qnew), Qold2);
        {empty, Qold1} ->
            queue:to_list(Qnew)
    end.

traverse_as_queue(Q) ->
    traverse_as_queue(queue:new(), Q).

traverse_as_queue(Qnew, Qold1) ->
    case queue:out(Qold1) of
        {{value, Item}, Qold2} ->
            traverse_as_queue(queue:in(Item, Qnew), Qold2);
        {empty, Qold1} ->
            Qnew
    end.

test() ->
    test(10000).

test(N) ->
    List = create(N),
    Queue = queue:from_list(List),
    {G1, _} = timer:tc(list_traversal, traverse_map, [List]),
    {G2, _} = timer:tc(list_traversal, traverse_reverse, [List]),
    {G3, _} = timer:tc(list_traversal, traverse_append, [List]),
    {G4, _} = timer:tc(list_traversal, traverse_queue, [List]),
    {G5, _} = timer:tc(list_traversal, traverse_as_queue, [Queue]),
    [
        #result{name = "lists:map/2",         get =  G1},
        #result{name = "reverse/traverse",    get =  G2},
        #result{name = "traverse/append",     get =  G3},
        #result{name = "list -> queue",       get =  G4},
        #result{name = "queue traverse",      get =  G5}
    ].

