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

-export([test/0, test/1, get/3, get_concurrent/2, set/3]).

-include("erlbench.hrl").

-define(WORDLIST, "words").

data1(_) ->
    gb_trees:empty().

data2(_) ->
    rbdict:new().

data3(_) ->
    aadict:new().

data4(_) ->
    orddict:new().

data5(_) ->
    dict:new().

data6(_) ->
    trie:new().

data7(_) ->
    ets:new(ets_test_1, []).

data8(_) ->
    undefined.

data9(_) ->
    ets:new(ets_test_2, [{read_concurrency, true}]).

data10(N) ->
    hasht:new(N).

data11(N) ->
    hashtl2:new(N).

data12(N) ->
    hashtl3:new(N).

data13(N) ->
    hashtl4:new(N).

data14(N) ->
    hashtl:new(N).

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

hasht_set(HashT, String, Value) ->
    hasht:store(String, Value, HashT).

hashtl2_set(HashT, String, Value) ->
    hashtl2:store(String, Value, HashT).

hashtl3_set(HashT, String, Value) ->
    hashtl3:store(String, Value, HashT).

hashtl4_set(HashT, String, Value) ->
    hashtl4:store(String, Value, HashT).

hashtl_set(HashT, String, Value) ->
    hashtl:store(String, Value, HashT).

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

hasht_get(HashT, String) ->
    hasht:fetch(String, HashT).

hashtl2_get(HashT, String) ->
    empty = hashtl2:fetch(String, HashT).

hashtl3_get(HashT, String) ->
    empty = hashtl3:fetch(String, HashT).

hashtl4_get(HashT, String) ->
    empty = hashtl4:fetch(String, HashT).

hashtl_get(HashT, String) ->
    empty = hashtl:fetch(String, HashT).

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
    {S1, D1} = timer:tc(string_key, set, [fun gb_trees_set/3, data1(N), Words]),
    {G1, _} = timer:tc(string_key, get, [fun gb_trees_get/2, D1, Words]),
    %% rbdict
    {S2, D2} = timer:tc(string_key, set, [fun rbdict_set/3, data2(N), Words]),
    {G2, _} = timer:tc(string_key, get, [fun rbdict_get/2, D2, Words]),
    %% aadict
    {S3, D3} = timer:tc(string_key, set, [fun aadict_set/3, data3(N), Words]),
    {G3, _} = timer:tc(string_key, get, [fun aadict_get/2, D3, Words]),
    %% orddict
    %{S4, D4} = timer:tc(string_key, set, [fun orddict_set/3, data4(N), Words]),
    %{G4, _} = timer:tc(string_key, get, [fun orddict_get/2, D4, Words]),
    %% dict
    {S5, D5} = timer:tc(string_key, set, [fun dict_set/3, data5(N), Words]),
    {G5, _} = timer:tc(string_key, get, [fun dict_get/2, D5, Words]),
    %% trie
    {S6, D6} = timer:tc(string_key, set, [fun trie_set/3, data6(N), Words]),
    {G6, _} = timer:tc(string_key, get, [fun trie_get/2, D6, Words]),
    %% ets
    {S7, D7} = timer:tc(string_key, set, [fun ets_set/3, data7(N), Words]),
    {G7, _} = timer:tc(string_key, get, [fun ets_get/2, D7, Words]),
    ets:delete(D7),
    %% process dictionary
    {S8, D8} = timer:tc(string_key, set, [fun pdict_set/3, data8(N), Words]),
    {G8, _} = timer:tc(string_key, get, [fun pdict_get/2, D8, Words]),
    %% ets with 10 concurrent accesses
    {_, D9} = timer:tc(string_key, set, [fun ets_set/3, data9(N), Words]),
    {G9, _} = timer:tc(string_key, get_concurrent, [10, [fun ets_get/2, D9, Words]]),
    ets:delete(D9),
    %% hash table
    {S10, D10} = timer:tc(string_key, set, [fun hasht_set/3, data10(N), Words]),
    {G10, _} = timer:tc(string_key, get, [fun hasht_get/2, D10, Words]),
    %% hash table layered
    {S11, D11} = timer:tc(string_key, set, [fun hashtl2_set/3, data11(N), Words]),
    {G11, _} = timer:tc(string_key, get, [fun hashtl2_get/2, D11, Words]),
    %% hash table layered
    {S12, D12} = timer:tc(string_key, set, [fun hashtl3_set/3, data12(N), Words]),
    {G12, _} = timer:tc(string_key, get, [fun hashtl3_get/2, D12, Words]),
    %% hash table layered
    {S13, D13} = timer:tc(string_key, set, [fun hashtl4_set/3, data13(N), Words]),
    {G13, _} = timer:tc(string_key, get, [fun hashtl4_get/2, D13, Words]),
    %% hash table layered
    {S14, D14} = timer:tc(string_key, set, [fun hashtl_set/3, data14(N), Words]),
    {G14, _} = timer:tc(string_key, get, [fun hashtl_get/2, D14, Words]),
    %% results
    [
        #result{name = "gb_trees",            get =  G1, set =  S1},
        #result{name = "rbdict",              get =  G2, set =  S2},
        #result{name = "aadict",              get =  G3, set =  S3},
        %#result{name = "orddict",             get =  G4, set =  S4},
        #result{name = "dict",                get =  G5, set =  S5},
        #result{name = "trie",                get =  G6, set =  S6},
        #result{name = "ets (set)",           get =  G7, set =  S7},
        #result{name = "process dictionary",  get =  G8, set =  S8},
        #result{name = "ets x10 (set)",       get = erlang:round(G9 / 10.0)},
        #result{name = "hasht",               get = G10, set = S10},
        #result{name = "hashtl2",             get = G11, set = S11},
        #result{name = "hashtl3",             get = G12, set = S12},
        #result{name = "hashtl4",             get = G13, set = S13},
        #result{name = "hashtl",              get = G14, set = S14}
    ].

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

