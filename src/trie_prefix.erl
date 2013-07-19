%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(trie_prefix).

-export([test/0, test/1, get/3, set/3]).

-include("erlbench.hrl").

-define(WORDLIST, "data/words").

-define(PREFIX, "0123456789").

data1(_) ->
    trie:new().

data2(_) ->
    trie:new().

exact_set(Trie, String, Value) ->
    trie:store(?PREFIX ++ String ++ "/" ++ String, Value, Trie).

pattern_set(Trie, _String, Value) ->
    trie:store(?PREFIX ++ "*/*", Value, Trie).

exact_get(Trie, String) ->
    {ok, _, Value} = trie:find_match(?PREFIX ++ String ++ "/" ++ String, Trie),
    Value.

pattern_get(Trie, String) ->
    {ok, _, Value} = trie:find_match(?PREFIX ++ String ++ "/" ++ String, Trie),
    Value.

get(_, _, []) ->
    ok;

get(Fun, Data, [H | T]) ->
    empty = Fun(Data, H),
    get(Fun, Data, T).

set(_, Data, []) ->
    Data;

set(Fun, Data, [H | T]) ->
    Data1 = Fun(Data, H, empty),
    set(Fun, Data1, T).

test() ->
    test(10000).

test(N) ->
    WordListLines = erlang:min(50000, N),
    Nfinal = N - N rem WordListLines,
    true = N == Nfinal,
    Words = lists:foldl(fun (_, L) ->
        array:to_list(array:resize(WordListLines, read_wordlist())) ++ L
    end, [], lists:seq(WordListLines, Nfinal, WordListLines)),
    true = erlang:length(Words) == Nfinal,

    %% exact
    {S1, D1} = timer:tc(?MODULE, set, [fun exact_set/3, data1(N), Words]),
    {G1, _} = timer:tc(?MODULE, get, [fun exact_get/2, D1, Words]),
    %% pattern
    {S2, D2} = timer:tc(?MODULE, set, [fun pattern_set/3, data2(N), Words]),
    {G2, _} = timer:tc(?MODULE, get, [fun pattern_get/2, D2, Words]),
    %% results
    [
        #result{name = "trie exact",          get =  G1, set =  S1},
        #result{name = "trie pattern",        get =  G2, set =  S2}
    ].

read_wordlist() ->
    {ok, F} = file:open(?WORDLIST, [read_ahead, raw, read]),
    Array = array:new([{size, 524288}, {default, -1}, {fixed, false}]),
    shuffle:shuffle(read_wordlist(0, F, Array)).

read_wordlist(I, F, Array) ->
    case file:read_line(F) of
        {ok, Line} ->
            Word = lists:sublist(Line, erlang:length(Line) - 1),
            if
                Word == "" ->
                    read_wordlist(I, F, Array);
                true ->
                    read_wordlist(I + 1, F, array:set(I, Word, Array))
            end;
        eof ->
            array:fix(array:resize(I, Array))
    end.

