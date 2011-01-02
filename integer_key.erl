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

-export([test/0, get/3, get_concurrent/2, set/3]).

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

data7(_) ->
    orddict:new().

data8(_) ->
    dict:new().

data9(_) ->
    ets:new(ets_test_1, []).

data10(_) ->
    undefined.

data11(_) ->
    ets:new(ets_test_2, [{read_concurrency, true}]).

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

orddict_set(Dict, I, Value) ->
    orddict:store(I, Value, Dict).

dict_set(Dict, I, Value) ->
    dict:store(I, Value, Dict).

ets_set(Tid, I, Value) ->
    true = ets:insert(Tid, {I, Value}),
    Tid.

pdict_set(_, I, Value) ->
    erlang:put(I, Value).

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

orddict_get(Dict, I) ->
    orddict:fetch(I, Dict).

dict_get(Dict, I) ->
    dict:fetch(I, Dict).

ets_get(Tid, I) ->
    ets:lookup_element(Tid, I, 2).

pdict_get(_, I) ->
    erlang:get(I).

get(_, _, 0) ->
    ok;

get(Fun, Data, N) ->
    true = N == Fun(Data, N),
    get(Fun, Data, N - 1).

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

set(_, Data, 0) ->
    Data;

set(Fun, Data, N) ->
    Data1 = Fun(Data, N, N),
    set(Fun, Data1, N - 1).

test() ->
    test(10000).

test(N) ->
    %% fixed-size array
    {S1, D1} = timer:tc(integer_key, set, [fun array_set/3, data1(N), N]),
    {G1, _} = timer:tc(integer_key, get, [fun array_get/2, D1, N]),
    %% extensible integer_keyay
    {S2, D2} = timer:tc(integer_key, set, [fun array_set/3, data2(N), N]),
    {G2, _} = timer:tc(integer_key, get, [fun array_get/2, D2, N]),
    %% tuple
    {S3, D3} = timer:tc(integer_key, set, [fun tuple_set/3, data3(N), N]),
    {G3, _} = timer:tc(integer_key, get, [fun tuple_get/2, D3, N]),
    %% gb_trees
    {S4, D4} = timer:tc(integer_key, set, [fun gb_trees_set/3, data4(N), N]),
    {G4, _} = timer:tc(integer_key, get, [fun gb_trees_get/2, D4, N]),
    %% rbdict
    {S5, D5} = timer:tc(integer_key, set, [fun rbdict_set/3, data5(N), N]),
    {G5, _} = timer:tc(integer_key, get, [fun rbdict_get/2, D5, N]),
    %% aadict
    {S6, D6} = timer:tc(integer_key, set, [fun aadict_set/3, data6(N), N]),
    {G6, _} = timer:tc(integer_key, get, [fun aadict_get/2, D6, N]),
    %% orddict
    {S7, D7} = timer:tc(integer_key, set, [fun orddict_set/3, data7(N), N]),
    {G7, _} = timer:tc(integer_key, get, [fun orddict_get/2, D7, N]),
    %% dict
    {S8, D8} = timer:tc(integer_key, set, [fun dict_set/3, data8(N), N]),
    {G8, _} = timer:tc(integer_key, get, [fun dict_get/2, D8, N]),
    %% ets
    {S9, D9} = timer:tc(integer_key, set, [fun ets_set/3, data9(N), N]),
    {G9, _} = timer:tc(integer_key, get, [fun ets_get/2, D9, N]),
    %% process dictionary
    {S10, D10} = timer:tc(integer_key, set, [fun pdict_set/3, data10(N), N]),
    {G10, _} = timer:tc(integer_key, get, [fun pdict_get/2, D10, N]),
    %% ets with 10 concurrent accesses
    {_, D11} = timer:tc(integer_key, set, [fun ets_set/3, data11(N), N]),
    {G11, _} = timer:tc(integer_key, get_concurrent, [10, [fun ets_get/2, D11, N]]),
    %% results
    io:format("N == ~8w~n", [N]),
    io:format("array (fixed):    get: ~8w µs, set: ~8w µs~n", [G1 , S1]),
    io:format("array (dynamic):  get: ~8w µs, set: ~8w µs~n", [G2 , S2]),
    io:format("tuple:            get: ~8w µs, set: ~8w µs~n", [G3 , S3]),
    io:format("gb_trees:         get: ~8w µs, set: ~8w µs~n", [G4 , S4]),
    io:format("rbdict:           get: ~8w µs, set: ~8w µs~n", [G5 , S5]),
    io:format("aadict:           get: ~8w µs, set: ~8w µs~n", [G6 , S6]),
    io:format("orddict:          get: ~8w µs, set: ~8w µs~n", [G7 , S7]),
    io:format("dict:             get: ~8w µs, set: ~8w µs~n", [G8 , S8]),
    io:format("ets (set):        get: ~8w µs, set: ~8w µs~n", [G9 , S9]),
    io:format("process dict:     get: ~8w µs, set: ~8w µs~n", [G10 , S10]),
    io:format("ets x10 (set):    get: ~8w µs~n", [erlang:round(G11 / 10.0)]),
    ok.

