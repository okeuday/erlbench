%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

% Data Structures For Integer Keys

% rbdict/aadict taken from:
% https://github.com/rvirding/rb/tree/develop/src

% run:
% erlc rbdict.erl
% erlc aadict.erl
% erlc integer_key.erl
% erl -noshell -s integer_key test -s init stop

% code modified from:
% http://www.wagerlabs.com/blog/2008/08/optimizing-erlang-a-death-match-of-arrays-vs-tuples.html
% http://groups.google.com/group/erlang-questions/browse_thread/thread/d171d2c4e997c29d

-module(integer_key).

-export([test/0, test/1, get/3, get_concurrent/2, set/3]).

-include("erlbench.hrl").

data1(N) ->
    %% size implies fixed-size array 
    %% but lets be explicit
    array:new([{size, N}, {default, 0}, {fixed, true}]).

data2(N) ->
    %% extensible array
    array:new([{size, N}, {default, -1}, {fixed, false}]).

data3(N) ->
    erlang:make_tuple(N, 0).

data4(_) ->
    gb_trees:empty().

data5(_) ->
    rbdict:new().

data6(_) ->
    aadict:new().

%data7(_) ->
%    orddict:new().

data8(_) ->
    dict:new().

data9(_) ->
    ets:new(ets_test_1, [set]).

data10(_) ->
    undefined.

data11(_) ->
    ets:new(ets_test_2, [set, {read_concurrency, true}]).

%data12(N) ->
%    hasht:new(N).

%data13(N) ->
%    hashtl2:new(N).

%data14(N) ->
%    hashtl3:new(N).

%data15(N) ->
%    hashtl4:new(N).

data16(N) ->
    hashtl:new(N).

data17(_) ->
    ets:new(ets_test_3, [ordered_set]).

data18(_) ->
    ets:new(ets_test_4, [ordered_set, {read_concurrency, true}]).

%data19(_) ->
%    ntree:new(10).

data20(_) ->
    btree7:new().

array_set(Array, I, Value) ->
    %% array indexing starts at 0
    array:set(I - 1, Value, Array).

tuple_set(Tuple, I, Value) ->
    %% tuple indexing starts at 1
    setelement(I, Tuple, Value).

gb_trees_set(Tree, I, Value) ->
    gb_trees:enter(I, Value, Tree).

rbdict_set(Dict, I, Value) ->
    rbdict:store(I, Value, Dict).

aadict_set(Dict, I, Value) ->
    aadict:store(I, Value, Dict).

%orddict_set(Dict, I, Value) ->
%    orddict:store(I, Value, Dict).

dict_set(Dict, I, Value) ->
    dict:store(I, Value, Dict).

ets_set(Tid, I, Value) ->
    true = ets:insert(Tid, {I, Value}),
    Tid.

pdict_set(_, I, Value) ->
    erlang:put(I, Value).

%hasht_set(HashT, I, Value) ->
%    hasht:store(I, Value, HashT).

%hashtl2_set(HashT, I, Value) ->
%    hashtl2:store(I, Value, HashT).

%hashtl3_set(HashT, I, Value) ->
%    hashtl3:store(I, Value, HashT).

%hashtl4_set(HashT, I, Value) ->
%    hashtl4:store(I, Value, HashT).

hashtl_set(HashT, I, Value) ->
    hashtl:store(I, Value, HashT).

%ntree_set(Tree, I, Value) ->
%    ntree:store(I, Value, Tree).

btree7_set(Tree, I, Value) ->
    btree7:store(I, Value, Tree).

array_get(Array, I) ->
    array:get(I - 1, Array).

tuple_get(Tuple, I) ->
    element(I, Tuple).

gb_trees_get(Tree, I) ->
    gb_trees:get(I, Tree).

rbdict_get(Dict, I) ->
    rbdict:fetch(I, Dict).

aadict_get(Dict, I) ->
    aadict:fetch(I, Dict).

%orddict_get(Dict, I) ->
%    orddict:fetch(I, Dict).

dict_get(Dict, I) ->
    dict:fetch(I, Dict).

ets_get(Tid, I) ->
    ets:lookup_element(Tid, I, 2).

pdict_get(_, I) ->
    erlang:get(I).

%hasht_get(HashT, I) ->
%    hasht:fetch(I, HashT).

%hashtl2_get(HashT, I) ->
%    I = hashtl2:fetch(I, HashT).

%hashtl3_get(HashT, I) ->
%    I = hashtl3:fetch(I, HashT).

%hashtl4_get(HashT, I) ->
%    I = hashtl4:fetch(I, HashT).

hashtl_get(HashT, I) ->
    I = hashtl:fetch(I, HashT).

%ntree_get(Tree, I) ->
%    {ok, V} = ntree:find(I, Tree),
%    V.

btree7_get(Tree, I) ->
    {ok, V} = btree7:find(I, Tree),
    V.

get(_, _, []) ->
    ok;

get(Fun, Data, [I | L]) ->
    true = I == Fun(Data, I),
    get(Fun, Data, L).

get_concurrent(Processes, Arguments) ->
    Parent = self(),
    Children = lists:map(fun(_) ->
        erlang:spawn(fun() ->
            ok = erlang:apply(integer_key, get, Arguments),
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

set(Fun, Data, [I | L]) ->
    Data1 = Fun(Data, I, I),
    set(Fun, Data1, L).

test() ->
    test(10000).

test(N) ->
    Integers = array:to_list(shuffle:shuffle(array:from_list(lists:seq(1, N)))),
    %% fixed-size array
    {S1, D1} = timer:tc(integer_key, set,
                        [fun array_set/3, data1(N), Integers]),
    {G1, _} = timer:tc(integer_key, get,
                       [fun array_get/2, D1, Integers]),
    %% extensible integer_keyay
    {S2, D2} = timer:tc(integer_key, set,
                        [fun array_set/3, data2(N), Integers]),
    {G2, _} = timer:tc(integer_key, get,
                       [fun array_get/2, D2, Integers]),
    %% tuple
    {S3, D3} = timer:tc(integer_key, set,
                        [fun tuple_set/3, data3(N), Integers]),
    {G3, _} = timer:tc(integer_key, get,
                       [fun tuple_get/2, D3, Integers]),
    %% gb_trees
    {S4, D4} = timer:tc(integer_key, set,
                        [fun gb_trees_set/3, data4(N), Integers]),
    {G4, _} = timer:tc(integer_key, get,
                       [fun gb_trees_get/2, D4, Integers]),
    %% rbdict
    {S5, D5} = timer:tc(integer_key, set,
                        [fun rbdict_set/3, data5(N), Integers]),
    {G5, _} = timer:tc(integer_key, get,
                       [fun rbdict_get/2, D5, Integers]),
    %% aadict
    {S6, D6} = timer:tc(integer_key, set,
                        [fun aadict_set/3, data6(N), Integers]),
    {G6, _} = timer:tc(integer_key, get,
                       [fun aadict_get/2, D6, Integers]),
    %% orddict
    %{S7, D7} = timer:tc(integer_key, set,
    %                    [fun orddict_set/3, data7(N), Integers]),
    %{G7, _} = timer:tc(integer_key, get,
    %                   [fun orddict_get/2, D7, Integers]),
    %% dict
    {S8, D8} = timer:tc(integer_key, set,
                        [fun dict_set/3, data8(N), Integers]),
    {G8, _} = timer:tc(integer_key, get,
                       [fun dict_get/2, D8, Integers]),
    %% ets
    {S9, D9} = timer:tc(integer_key, set,
                        [fun ets_set/3, data9(N), Integers]),
    {G9, _} = timer:tc(integer_key, get,
                       [fun ets_get/2, D9, Integers]),
    ets:delete(D9),
    %% process dictionary
    {S10, D10} = timer:tc(integer_key, set,
                          [fun pdict_set/3, data10(N), Integers]),
    {G10, _} = timer:tc(integer_key, get,
                        [fun pdict_get/2, D10, Integers]),
    %% ets with 10 concurrent accesses
    %{_, D11} = timer:tc(integer_key, set,
    %                    [fun ets_set/3, data11(N), Integers]),
    %{G11, _} = timer:tc(integer_key, get_concurrent,
    %                    [10, [fun ets_get/2, D11, Integers]]),
    %ets:delete(D11),
    %% hash table
    %{S12, D12} = timer:tc(integer_key, set,
    %                      [fun hasht_set/3, data12(N), Integers]),
    %{G12, _} = timer:tc(integer_key, get,
    %                    [fun hasht_get/2, D12, Integers]),
    %% hash table layered
    %{S13, D13} = timer:tc(integer_key, set,
    %                      [fun hashtl2_set/3, data13(N), Integers]),
    %{G13, _} = timer:tc(integer_key, get,
    %                    [fun hashtl2_get/2, D13, Integers]),
    %% hash table layered
    %{S14, D14} = timer:tc(integer_key, set,
    %                      [fun hashtl3_set/3, data14(N), Integers]),
    %{G14, _} = timer:tc(integer_key, get,
    %                    [fun hashtl3_get/2, D14, Integers]),
    %% hash table layered
    %{S15, D15} = timer:tc(integer_key, set,
    %                      [fun hashtl4_set/3, data15(N), Integers]),
    %{G15, _} = timer:tc(integer_key, get,
    %                    [fun hashtl4_get/2, D15, Integers]),
    %% hash table layered
    {S16, D16} = timer:tc(integer_key, set,
                          [fun hashtl_set/3, data16(N), Integers]),
    {G16, _} = timer:tc(integer_key, get,
                        [fun hashtl_get/2, D16, Integers]),
    %% ets
    {S17, D17} = timer:tc(integer_key, set,
                          [fun ets_set/3, data17(N), Integers]),
    {G17, _} = timer:tc(integer_key, get,
                        [fun ets_get/2, D17, Integers]),
    ets:delete(D17),
    %% ets with 10 concurrent accesses
    %{_, D18} = timer:tc(integer_key, set,
    %                    [fun ets_set/3, data18(N), Integers]),
    %{G18, _} = timer:tc(integer_key, get_concurrent,
    %                    [10, [fun ets_get/2, D18, Integers]]),
    %ets:delete(D18),
    %{S19, D19} = timer:tc(integer_key, set,
    %                      [fun ntree_set/3, data19(N), Integers]),
    %{G19, _} = timer:tc(integer_key, get,
    %                    [fun ntree_get/2, D19, Integers]),
    {S20, D20} = timer:tc(integer_key, set,
                          [fun btree7_set/3, data20(N), Integers]),
    {G20, _} = timer:tc(integer_key, get,
                        [fun btree7_get/2, D20, Integers]),
    %% results
    [
        #result{name = "array (fixed)",       get =  G1, set =  S1},
        #result{name = "array (dynamic)",     get =  G2, set =  S2},
        #result{name = "tuple",               get =  G3, set =  S3},
        #result{name = "gb_trees",            get =  G4, set =  S4},
        #result{name = "rbdict",              get =  G5, set =  S5},
        #result{name = "aadict",              get =  G6, set =  S6},
        %#result{name = "orddict",             get =  G7, set =  S7},
        #result{name = "dict",                get =  G8, set =  S8},
        #result{name = "ets (set)",           get =  G9, set =  S9},
        #result{name = "process dictionary",  get = G10, set = S10},
        %#result{name = "ets x10 read (set)",  get = erlang:round(G11 / 10.0)},
        %#result{name = "hasht",               get = G12, set = S12},
        %#result{name = "hashtl2",             get = G13, set = S13},
        %#result{name = "hashtl3",             get = G14, set = S14},
        %#result{name = "hashtl4",             get = G15, set = S15},
        #result{name = "hashtl",              get = G16, set = S16},
        #result{name = "ets (ordered_set)",   get = G17, set = S17},
        %#result{name = "ets x10 read (ordered_set)", get = erlang:round(G18 / 10.0)},
        %#result{name = "ntree",               get = G19, set = S19},
        #result{name = "btree7",              get = G20, set = S20}
    ].

