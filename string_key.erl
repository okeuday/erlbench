%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

% Data Structures For String (List of Integers) Keys

% rbdict/aadict taken from:
% https://github.com/rvirding/rb/tree/develop/src

% trie taken from:
% https://github.com/okeuday/CloudI/blob/master/src/lib/cloud_stdlib/src/trie.erl

% run:
% erlc rbdict.erl
% erlc aadict.erl
% erlc trie.erl
% erlc string_key.erl
% erl -noshell -s string_key test -s init stop

-module(string_key).

-export([test/0, get/3, get_concurrent/2, set/3]).

-define(WORDLIST, "/usr/share/dict/words").

data1() ->
    gb_trees:empty().

data2() ->
    rbdict:new().

data3() ->
    aadict:new().

data4() ->
    orddict:new().

data5() ->
    dict:new().

data6() ->
    trie:new().

data7() ->
    ets:new(ets_test_1, []).

data8() ->
    undefined.

data9() ->
    ets:new(ets_test_2, [{read_concurrency, true}]).

gb_trees_set(Tree, String, Value) ->
    gb_trees:enter(String, Value, Tree).

rbdict_set(Dict, String, Value) ->
    rbdict:store(String, Value, Dict).

aadict_set(Dict, String, Value) ->
    aadict:store(String, Value, Dict).

orddict_set(Dict, String, Value) ->
    orddict:store(String, Value, Dict).

dict_set(Dict, String, Value) ->
    dict:store(String, Value, Dict).

trie_set(Trie, String, Value) ->
    trie:store(String, Value, Trie).

ets_set(Tid, String, Value) ->
    true = ets:insert(Tid, {String, Value}),
    Tid.

pdict_set(_, String, Value) ->
    erlang:put(String, Value).

gb_trees_get(Tree, String) ->
    gb_trees:get(String, Tree).

rbdict_get(Dict, String) ->
    rbdict:fetch(String, Dict).

aadict_get(Dict, String) ->
    aadict:fetch(String, Dict).

orddict_get(Dict, String) ->
    orddict:fetch(String, Dict).

dict_get(Dict, String) ->
    dict:fetch(String, Dict).

trie_get(Trie, String) ->
    trie:fetch(String, Trie).

ets_get(Tid, String) ->
    ets:lookup_element(Tid, String, 2).

pdict_get(_, String) ->
    erlang:get(String).

get(_, _, []) ->
    ok;

get(Fun, Data, [H | T]) ->
    empty = Fun(Data, H),
    get(Fun, Data, T).

get_concurrent(Processes, Arguments) ->
    Parent = self(),
    Children = lists:map(fun(_) ->
        erlang:spawn(fun() ->
            ok = erlang:apply(string_key, get, Arguments),
            Parent ! {self(), done}
        end)
    end, lists:seq(1, Processes)),
    lists:foreach(fun(Child) ->
        receive
            {Child, done} ->
                ok
        end
    end, Children),
    ok.

set(_, Data, []) ->
    Data;

set(Fun, Data, [H | T]) ->
    Data1 = Fun(Data, H, empty),
    set(Fun, Data1, T).

test() ->
    test(10000).

test(N) ->
    Words = array:to_list(array:resize(N, read_wordlist())),

    %% gb_trees
    {S1, D1} = timer:tc(string_key, set, [fun gb_trees_set/3, data1(), Words]),
    {G1, _} = timer:tc(string_key, get, [fun gb_trees_get/2, D1, Words]),
    %% rbdict
    {S2, D2} = timer:tc(string_key, set, [fun rbdict_set/3, data2(), Words]),
    {G2, _} = timer:tc(string_key, get, [fun rbdict_get/2, D2, Words]),
    %% aadict
    {S3, D3} = timer:tc(string_key, set, [fun aadict_set/3, data3(), Words]),
    {G3, _} = timer:tc(string_key, get, [fun aadict_get/2, D3, Words]),
    %% orddict
    {S4, D4} = timer:tc(string_key, set, [fun orddict_set/3, data4(), Words]),
    {G4, _} = timer:tc(string_key, get, [fun orddict_get/2, D4, Words]),
    %% dict
    {S5, D5} = timer:tc(string_key, set, [fun dict_set/3, data5(), Words]),
    {G5, _} = timer:tc(string_key, get, [fun dict_get/2, D5, Words]),
    %% trie
    {S6, D6} = timer:tc(string_key, set, [fun trie_set/3, data6(), Words]),
    {G6, _} = timer:tc(string_key, get, [fun trie_get/2, D6, Words]),
    %% ets
    {S7, D7} = timer:tc(string_key, set, [fun ets_set/3, data7(), Words]),
    {G7, _} = timer:tc(string_key, get, [fun ets_get/2, D7, Words]),
    %% process dictionary
    {S8, D8} = timer:tc(string_key, set, [fun pdict_set/3, data8(), Words]),
    {G8, _} = timer:tc(string_key, get, [fun pdict_get/2, D8, Words]),
    %% ets with 10 concurrent accesses
    {_, D9} = timer:tc(string_key, set, [fun ets_set/3, data9(), Words]),
    {G9, _} = timer:tc(string_key, get_concurrent, [10, [fun ets_get/2, D9, Words]]),
    %% results
    io:format("N == ~8w~n", [N]),
    io:format("gb_trees:         get: ~8w µs, set: ~8w µs~n", [G1, S1]),
    io:format("rbdict:           get: ~8w µs, set: ~8w µs~n", [G2, S2]),
    io:format("aadict:           get: ~8w µs, set: ~8w µs~n", [G3, S3]),
    io:format("orddict:          get: ~8w µs, set: ~8w µs~n", [G4, S4]),
    io:format("dict:             get: ~8w µs, set: ~8w µs~n", [G5, S5]),
    io:format("trie:             get: ~8w µs, set: ~8w µs~n", [G6, S6]),
    io:format("ets (set):        get: ~8w µs, set: ~8w µs~n", [G7, S7]),
    io:format("process dict:     get: ~8w µs, set: ~8w µs~n", [G8, S8]),
    io:format("ets x10 (set):    get: ~8w µs~n", [erlang:round(G9 / 10.0)]),
    ok.

read_wordlist() ->
    {ok, F} = file:open(?WORDLIST, [read_ahead, raw, read]),
    Array = array:new([{size, 524288}, {default, -1}, {fixed, false}]),
    shuffle(read_wordlist(0, F, Array)).

read_wordlist(I, F, Array) ->
    case file:read_line(F) of
        {ok, Line} ->
            Word = lists:sublist(Line, erlang:length(Line) - 1),
            read_wordlist(I + 1, F, array:set(I, Word, Array));
        eof ->
            array:fix(array:resize(I, Array))
    end.

shuffle(Array) ->
    % Fisher-Yates shuffle
    shuffle(array:size(Array) - 1, Array).

shuffle(0, Array) ->
    Array;

shuffle(I, Array) ->
    J = random:uniform(I + 1) - 1,
    Temp = array:get(I, Array),
    NewArray = array:set(J, Temp, array:set(I, array:get(J, Array), Array)),
    shuffle(I - 1, NewArray).

