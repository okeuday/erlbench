%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

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

-module(binary_key).

-export([test/0, test/1, get/3, get_concurrent/2, set/3]).

-include("erlbench.hrl").

-define(WORDLIST, "data/words").

data1(_) ->
    gb_trees:empty().

data2(_) ->
    rbdict:new().

data3(_) ->
    aadict:new().

%data4(_) ->
%    orddict:new().

data5(_) ->
    dict:new().

data7(_) ->
    ets:new(ets_test_1, [set]).

data8(_) ->
    undefined.

data9(_) ->
    ets:new(ets_test_2, [set, {read_concurrency, true}]).

%data10(N) ->
%    hasht:new(N).

%data11(N) ->
%    hashtl2:new(N).

%data12(N) ->
%    hashtl3:new(N).

%data13(N) ->
%    hashtl4:new(N).

data14(N) ->
    hashtl:new(N).

data15(_) ->
    ets:new(ets_test_3, [ordered_set]).

data16(_) ->
    ets:new(ets_test_4, [ordered_set, {read_concurrency, true}]).

data17(_) ->
    btrie:new().

data18(_) ->
    hashdict:new().

data19(_) ->
    maps:new().

data20(_) ->
    blookupv:new().

data21(_) ->
    blookupf:new(32).

gb_trees_set(Tree, String, Value) ->
    gb_trees:enter(String, Value, Tree).

rbdict_set(Dict, String, Value) ->
    rbdict:store(String, Value, Dict).

aadict_set(Dict, String, Value) ->
    aadict:store(String, Value, Dict).

%orddict_set(Dict, String, Value) ->
%    orddict:store(String, Value, Dict).

dict_set(Dict, String, Value) ->
    dict:store(String, Value, Dict).

ets_set(Tid, String, Value) ->
    true = ets:insert(Tid, {String, Value}),
    Tid.

pdict_set(_, String, Value) ->
    erlang:put(String, Value).

%hasht_set(HashT, String, Value) ->
%    hasht:store(String, Value, HashT).

%hashtl2_set(HashT, String, Value) ->
%    hashtl2:store(String, Value, HashT).

%hashtl3_set(HashT, String, Value) ->
%    hashtl3:store(String, Value, HashT).

%hashtl4_set(HashT, String, Value) ->
%    hashtl4:store(String, Value, HashT).

hashtl_set(HashT, String, Value) ->
    hashtl:store(String, Value, HashT).

btrie_set(Trie, String, Value) ->
    btrie:store(String, Value, Trie).

hashdict_set(Dict, String, Value) ->
    hashdict:store(String, Value, Dict).

maps_set(Map, String, Value) ->
    maps:put(String, Value, Map).

blookupv_set(Lookup, String, empty) ->
    blookupv:store(String, <<>>, Lookup).

blookupf_set(Lookup, String, empty) ->
    blookupf:store(String, <<>>, Lookup).

gb_trees_get(Tree, String) ->
    gb_trees:get(String, Tree).

rbdict_get(Dict, String) ->
    rbdict:fetch(String, Dict).

aadict_get(Dict, String) ->
    aadict:fetch(String, Dict).

%orddict_get(Dict, String) ->
%    orddict:fetch(String, Dict).

dict_get(Dict, String) ->
    dict:fetch(String, Dict).

ets_get(Tid, String) ->
    ets:lookup_element(Tid, String, 2).

pdict_get(_, String) ->
    erlang:get(String).

%hasht_get(HashT, String) ->
%    hasht:fetch(String, HashT).

%hashtl2_get(HashT, String) ->
%    empty = hashtl2:fetch(String, HashT).

%hashtl3_get(HashT, String) ->
%    empty = hashtl3:fetch(String, HashT).

%hashtl4_get(HashT, String) ->
%    empty = hashtl4:fetch(String, HashT).

hashtl_get(HashT, String) ->
    empty = hashtl:fetch(String, HashT).

btrie_get(Trie, String) ->
    btrie:fetch(String, Trie).

hashdict_get(Dict, String) ->
    hashdict:fetch(String, Dict).

maps_get(Map, String) ->
    {ok, Value} = maps:find(String, Map),
    Value.

blookupv_get(Lookup, String) ->
    {ok, <<>>} = blookupv:find(String, Lookup),
    empty.

blookupf_get(Lookup, String) ->
    {ok, <<>>} = blookupf:find(String, Lookup),
    empty.

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
    WordListLines = erlang:min(50000, N),
    Nfinal = N - N rem WordListLines,
    true = N == Nfinal,
    Words = lists:foldl(fun (_, L) ->
        array:to_list(array:resize(WordListLines, read_wordlist())) ++ L
    end, [], lists:seq(WordListLines, Nfinal, WordListLines)),
    true = erlang:length(Words) == Nfinal,

    %% gb_trees
    {S1, D1} = timer:tc(?MODULE, set, [fun gb_trees_set/3, data1(N), Words]),
    {G1, _} = timer:tc(?MODULE, get, [fun gb_trees_get/2, D1, Words]),
    %% rbdict
    {S2, D2} = timer:tc(?MODULE, set, [fun rbdict_set/3, data2(N), Words]),
    {G2, _} = timer:tc(?MODULE, get, [fun rbdict_get/2, D2, Words]),
    %% aadict
    {S3, D3} = timer:tc(?MODULE, set, [fun aadict_set/3, data3(N), Words]),
    {G3, _} = timer:tc(?MODULE, get, [fun aadict_get/2, D3, Words]),
    %% orddict
    %{S4, D4} = timer:tc(?MODULE, set, [fun orddict_set/3, data4(N), Words]),
    %{G4, _} = timer:tc(?MODULE, get, [fun orddict_get/2, D4, Words]),
    %% dict
    {S5, D5} = timer:tc(?MODULE, set, [fun dict_set/3, data5(N), Words]),
    {G5, _} = timer:tc(?MODULE, get, [fun dict_get/2, D5, Words]),
    %% ets
    {S7, D7} = timer:tc(?MODULE, set, [fun ets_set/3, data7(N), Words]),
    {G7, _} = timer:tc(?MODULE, get, [fun ets_get/2, D7, Words]),
    ets:delete(D7),
    %% process dictionary
    {S8, D8} = timer:tc(?MODULE, set, [fun pdict_set/3, data8(N), Words]),
    {G8, _} = timer:tc(?MODULE, get, [fun pdict_get/2, D8, Words]),
    %% ets with 10 concurrent accesses
    {_, D9} = timer:tc(?MODULE, set, [fun ets_set/3, data9(N), Words]),
    {G9, _} = timer:tc(?MODULE, get_concurrent, [10, [fun ets_get/2, D9, Words]]),
    ets:delete(D9),
    %% hash table
    %{S10, D10} = timer:tc(?MODULE, set, [fun hasht_set/3, data10(N), Words]),
    %{G10, _} = timer:tc(?MODULE, get, [fun hasht_get/2, D10, Words]),
    %% hash table layered
    %{S11, D11} = timer:tc(?MODULE, set, [fun hashtl2_set/3, data11(N), Words]),
    %{G11, _} = timer:tc(?MODULE, get, [fun hashtl2_get/2, D11, Words]),
    %% hash table layered
    %{S12, D12} = timer:tc(?MODULE, set, [fun hashtl3_set/3, data12(N), Words]),
    %{G12, _} = timer:tc(?MODULE, get, [fun hashtl3_get/2, D12, Words]),
    %% hash table layered
    %{S13, D13} = timer:tc(?MODULE, set, [fun hashtl4_set/3, data13(N), Words]),
    %{G13, _} = timer:tc(?MODULE, get, [fun hashtl4_get/2, D13, Words]),
    %% hash table layered
    {S14, D14} = timer:tc(?MODULE, set, [fun hashtl_set/3, data14(N), Words]),
    {G14, _} = timer:tc(?MODULE, get, [fun hashtl_get/2, D14, Words]),
    %% ets
    {S15, D15} = timer:tc(?MODULE, set, [fun ets_set/3, data15(N), Words]),
    {G15, _} = timer:tc(?MODULE, get, [fun ets_get/2, D15, Words]),
    ets:delete(D15),
    %% ets with 10 concurrent accesses
    {_, D16} = timer:tc(?MODULE, set, [fun ets_set/3, data16(N), Words]),
    {G16, _} = timer:tc(?MODULE, get_concurrent, [10, [fun ets_get/2, D16, Words]]),
    ets:delete(D16),
    % btrie
    {S17, D17} = timer:tc(?MODULE, set, [fun btrie_set/3, data17(N), Words]),
    {G17, _} = timer:tc(?MODULE, get, [fun btrie_get/2, D17, Words]),
    % hashdict
    {S18, D18} = timer:tc(?MODULE, set, [fun hashdict_set/3, data18(N), Words]),
    {G18, _} = timer:tc(?MODULE, get, [fun hashdict_get/2, D18, Words]),
    % map in Erlang/OTP 18.0
    {S19, D19} = timer:tc(?MODULE, set, [fun maps_set/3, data19(N), Words]),
    {G19, _} = timer:tc(?MODULE, get, [fun maps_get/2, D19, Words]),
    % blookupv
    {S20a, D20} = timer:tc(?MODULE, set, [fun blookupv_set/3, data20(N), Words]),
    {S20b, _} = timer:tc(?MODULE, set, [fun blookupv_set/3, D20, Words]),
    {G20, _} = timer:tc(?MODULE, get, [fun blookupv_get/2, D20, Words]),
    % blookupf
    {S21, D21} = timer:tc(?MODULE, set, [fun blookupf_set/3, data21(N), Words]),
    {G21, _} = timer:tc(?MODULE, get, [fun blookupf_get/2, D21, Words]),
    %% results
    [
        #result{name = "gb_trees",            get =  G1, set =  S1},
        #result{name = "rbdict",              get =  G2, set =  S2},
        #result{name = "aadict",              get =  G3, set =  S3},
        %#result{name = "orddict",             get =  G4, set =  S4},
        #result{name = "dict",                get =  G5, set =  S5},
        #result{name = "ets (set)",           get =  G7, set =  S7},
        #result{name = "process dictionary",  get =  G8, set =  S8},
        #result{name = "ets x10 read (set)",  get = erlang:round(G9 / 10.0)},
        %#result{name = "hasht",               get = G10, set = S10},
        %#result{name = "hashtl2",             get = G11, set = S11},
        %#result{name = "hashtl3",             get = G12, set = S12},
        %#result{name = "hashtl4",             get = G13, set = S13},
        #result{name = "hashtl",              get = G14, set = S14},
        #result{name = "ets (ordered_set)",   get = G15, set = S15},
        #result{name = "ets x10 read (ordered_set)",
                get = erlang:round(G16 / 10.0)},
        #result{name = "btrie",               get = G17, set = S17},
        #result{name = "hashdict",            get = G18, set = S18},
        #result{name = "map",                 get = G19, set = S19},
        #result{name = "blookupv",            get = G20, set = S20a,
                                              update = S20b},
        #result{name = "blookupf",            get = G21, set = S21}
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
                    read_wordlist(I + 1, F,
                        array:set(I, erlang:list_to_binary(Word), Array))
            end;
        eof ->
            array:fix(array:resize(I, Array))
    end.

