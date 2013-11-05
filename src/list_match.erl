%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(list_match).

-export([test/0, test/1,
         match_characters/1,
         match_list/1]).

-include("erlbench.hrl").


create(N) ->
    "prefix_" ++
    erlang:binary_to_list(base64:encode(crypto:strong_rand_bytes(N))).

match_characters(L) ->
    [$p, $r, $e, $f, $i, $x, $_ | Rest] = L,
    Rest.
 
match_list(L) ->
    "prefix_" ++ Rest = L,
    Rest.

test() ->
    test(10000).

test(N) ->
    List = create(N),
    {G1, _} = timer:tc(?MODULE, match_characters, [List]),
    {G2, _} = timer:tc(?MODULE, match_list, [List]),
    [
        #result{name = "[C1, ...] =",         get =  G1},
        #result{name = "\"...\" ++ ... =",    get =  G2}
    ].

