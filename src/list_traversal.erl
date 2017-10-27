%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(list_traversal).

-export([test/0, test/1,
         traverse_map/1,
         traverse_reverse/1,
         traverse_append/1,
         traverse_queue/1,
         traverse_as_queue/1,
         traverse_comprehension/1,
         traverse_foldr/1]).

-include("erlbench.hrl").


create(N) ->
    lists:seq(1, N).

traverse_map(L) ->
    lists:map(fun(I) -> I + 1 end, L).

traverse_reverse(L) ->
    traverse_reverse([], lists:reverse(L)).
    
traverse_reverse(L, []) ->
    L;
traverse_reverse(L, [H | T]) ->
    traverse_reverse([(H + 1) | L], T).

traverse_append(L) ->
    traverse_append([], L).

traverse_append(L, []) ->
    L;
traverse_append(L, [H | T]) ->
    traverse_append(L ++ [H + 1], T).

traverse_queue(L) ->
    traverse_queue(queue:new(), queue:from_list(L)).

traverse_queue(Qnew, Qold1) ->
    case queue:out(Qold1) of
        {{value, Item}, Qold2} ->
            traverse_queue(queue:in(Item + 1, Qnew), Qold2);
        {empty, Qold1} ->
            queue:to_list(Qnew)
    end.

traverse_as_queue(Q) ->
    traverse_as_queue(queue:new(), Q).

traverse_as_queue(Qnew, Qold1) ->
    case queue:out(Qold1) of
        {{value, Item}, Qold2} ->
            traverse_as_queue(queue:in(Item + 1, Qnew), Qold2);
        {empty, Qold1} ->
            Qnew
    end.

traverse_comprehension(L) ->
    Result = [I + 1 || I <- L],
    Result.

traverse_foldr(L) ->
    lists:foldr(fun(I, Lnew) -> [(I + 1) | Lnew] end, [], L).

test() ->
    test(10000).

test(N) ->
    List = create(N),
    Queue = queue:from_list(List),
    {G1, _} = timer:tc(?MODULE, traverse_map, [List]),
    {G2, _} = timer:tc(?MODULE, traverse_reverse, [List]),
    {G3, _} = timer:tc(?MODULE, traverse_append, [List]),
    {G4, _} = timer:tc(?MODULE, traverse_queue, [List]),
    {G5, _} = timer:tc(?MODULE, traverse_as_queue, [Queue]),
    {G6, _} = timer:tc(?MODULE, traverse_comprehension, [List]),
    {G7, _} = timer:tc(?MODULE, traverse_foldr, [List]),
    [
        #result{name = "lists:map/2",         get =  G1},
        #result{name = "reverse/traverse",    get =  G2},
        #result{name = "traverse/append",     get =  G3},
        #result{name = "list -> queue",       get =  G4},
        #result{name = "queue traverse",      get =  G5},
        #result{name = "list comprehension",  get =  G6},
        #result{name = "lists:foldr/3",       get =  G7}
    ].

