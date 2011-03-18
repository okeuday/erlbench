%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(pseudo_randomness).

-export([test/1, run/2]).

-include("erlbench.hrl").

test_now() ->
    {_, _, I} = erlang:now(),
    I rem 11.

test_crypto() ->
    crypto:rand_uniform(1, 11).

test_random() ->
    random:uniform(10).

test_reductions1() ->
    % not very random
    {reductions, I} = erlang:process_info(self(), reductions),
    I rem 11.

test_reductions2() ->
    % not very random
    {I1, I2} = erlang:statistics(reductions),
    (I1 bxor I2) rem 11.

test_stats_io() ->
    % not very random
    {{input, I1},{output, I2}} = erlang:statistics(io),
    (I1 bxor I2) rem 11.
    
run(1, F) ->
    true = F() =< 10;
run(N, F) ->
    true = F() =< 10,
    run(N - 1, F).

test(N) ->
    {Test1, _} = timer:tc(pseudo_randomness, run, [N, fun test_now/0]),
    {Test2, _} = timer:tc(pseudo_randomness, run, [N, fun test_crypto/0]),
    {Test3, _} = timer:tc(pseudo_randomness, run, [N, fun test_random/0]),
    {Test4, _} = timer:tc(pseudo_randomness, run, [N, fun test_reductions1/0]),
    {Test5, _} = timer:tc(pseudo_randomness, run, [N, fun test_reductions2/0]),
    {Test6, _} = timer:tc(pseudo_randomness, run, [N, fun test_stats_io/0]),

    %% results
    [
        #result{name = "erlang:now/0",               get =  Test1},
        #result{name = "crypto:rand_uniform/2",      get =  Test2},
        #result{name = "random:uniform/1",           get =  Test3},
        #result{name = "erlang:process_info(,r)",    get =  Test4},
        #result{name = "erlang:statistics(r)",       get =  Test5},
        #result{name = "erlang:statistics(io)",      get =  Test6}
    ].

