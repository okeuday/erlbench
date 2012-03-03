%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(pseudo_randomness).

-export([test/1, run/2]).

-include("erlbench.hrl").

%-define(PRINT_DISTRIBUTION, true).

test_now() ->
    {_, _, I} = erlang:now(),
    (I rem 10) + 1.

test_crypto() ->
    crypto:rand_uniform(1, 11).

test_random() ->
    random:uniform(10).

test_random_wh06() ->
    random_wh06:uniform(10).

test_reductions1() ->
    % not very uniform
    {reductions, I} = erlang:process_info(self(), reductions),
    (I rem 10) + 1.

test_reductions2() ->
    % not very uniform in the test
    {I1, I2} = erlang:statistics(reductions),
    ((I1 bxor I2) rem 10) + 1.

test_stats_io() ->
    % not random at all, excluded from test
    {{input, I1},{output, I2}} = erlang:statistics(io),
    ((I1 bxor I2) rem 10) + 1.

-ifdef(PRINT_DISTRIBUTION).
counts_init() ->
    lists:foreach(fun(I) ->
        erlang:put(I, 0)
    end, lists:seq(1, 10)).
-else.
counts_init() ->
    ok.
-endif.

-ifdef(PRINT_DISTRIBUTION).
counts_incr(I) ->
    erlang:put(I, erlang:get(I) + 1).
-else.
counts_incr(_) ->
    ok.
-endif.

-ifdef(PRINT_DISTRIBUTION).
counts_print(Title) ->
    io:format("~s~n", [Title]),
    lists:foreach(fun(I) ->
        io:format("~10w: ~w~n", [I, erlang:get(I)])
    end, lists:seq(1, 10)).
-else.
counts_print(_) ->
    ok.
-endif.
    
run(1, F) ->
    Value = F(),
    counts_incr(Value),
    true = Value =< 10,
    Value;
run(N, F) ->
    Value = F(),
    counts_incr(Value),
    true = Value =< 10,
    run(N - 1, F).

test(N) ->
    counts_init(),
    {Test1, _} = timer:tc(pseudo_randomness, run, [N, fun test_now/0]),
    counts_print("erlang:now/0"),
    counts_init(),
    {Test2, _} = timer:tc(pseudo_randomness, run, [N, fun test_crypto/0]),
    counts_print("crypto:rand_uniform/2"),
    counts_init(),
    {Test3, _} = timer:tc(pseudo_randomness, run, [N, fun test_random/0]),
    counts_print("random:uniform/1"),
    % not uniform
    %counts_init(),
    %{Test4, _} = timer:tc(pseudo_randomness, run, [N, fun test_reductions1/0]),
    %counts_print("erlang:process_info(self(), reductions)"),
    %counts_init(),
    %{Test5, _} = timer:tc(pseudo_randomness, run, [N, fun test_reductions2/0]),
    %counts_print("erlang:statistics(reductions)"),
    counts_init(),
    {Test6, _} = timer:tc(pseudo_randomness, run, [N, fun test_random_wh06/0]),
    counts_print("random:uniform_wh06/1"),

    %% results
    [
        #result{name = "erlang:now/0",               get =  Test1},
        #result{name = "crypto:rand_uniform/2",      get =  Test2},
        #result{name = "random:uniform/1",           get =  Test3},
        %#result{name = "erlang:process_info(,r)",    get =  Test4},
        %#result{name = "erlang:statistics(r)",       get =  Test5},
        #result{name = "random:uniform_wh06/1",      get =  Test6}
    ].

