%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(pseudo_randomness).

-export([test/1, run/2, run_s/3]).

-include("erlbench.hrl").

-define(PRINT_DISTRIBUTION, true).

test_18_bxor_abs() ->
    I = erlang:abs(erlang:monotonic_time() bxor erlang:unique_integer()),
    (I rem 10) + 1.

test_18_monotonic() ->
    I = erlang:abs(erlang:monotonic_time()),
    (I rem 10) + 1.

test_18_unique() ->
    % not uniform at all, excluded from test
    I = erlang:unique_integer([positive]),
    (I rem 10) + 1.

test_18_rand() ->
    rand:uniform(10).

%test_now() ->
%    % most uniform solution
%    {_, _, I} = erlang:now(),
%    (I rem 10) + 1.

test_crypto() ->
    crypto:rand_uniform(1, 11).

test_random() ->
    random:uniform(10).

test_random_wh82() ->
    random_wh82:uniform(10).

test_random_wh82_int() ->
    random_wh82_int:uniform(10).

test_random_wh06_int() ->
    random_wh06_int:uniform(10).

test_reductions1() ->
    % not uniform
    {reductions, I} = erlang:process_info(self(), reductions),
    (I rem 10) + 1.

test_reductions2() ->
    % not uniform
    {I1, I2} = erlang:statistics(reductions),
    ((I1 bxor I2) rem 10) + 1.

test_stats_io() ->
    % not random at all, excluded from test
    {{input, I1},{output, I2}} = erlang:statistics(io),
    ((I1 bxor I2) rem 10) + 1.

test_timestamp() ->
    {_, _, I} = os:timestamp(),
    (I rem 10) + 1.

test_18_erlang_timestamp() ->
    I = erlang:system_time(micro_seconds),
    (I rem 10) + 1.

test_18_os_system_time() ->
    I = os:system_time(micro_seconds),
    (I rem 10) + 1.

test_garbage_collections() ->
    % super slow
    {I1, I2, I3} = erlang:statistics(garbage_collection),
    ((I1 bxor I2 bxor I3) rem 10) + 1.

test_context_switches() ->
    % not uniform
    {I1, I2} = erlang:statistics(context_switches),
    ((I1 bxor I2) rem 10) + 1.

test_make_ref() ->
    % not uniform, but quickest
    erlang:phash2(erlang:make_ref(), 10) + 1.

test_19_perf_counter() ->
    I = os:perf_counter(micro_seconds),
    (I rem 10) + 1.

test_os_time_perf_counter() ->
    I = os_time:perf_counter(),
    (I rem 10) + 1.

test_quickrand_cache_uniform() ->
    quickrand_cache:uniform(10).

test_quickrand_cache_uniform(State) ->
    quickrand_cache:uniform(10, State).

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
    
run(0, _) ->
    ok;
run(N, F) ->
    Value = F(),
    counts_incr(Value),
    true = Value =< 10,
    run(N - 1, F).

run_s(0, _, _) ->
    ok;
run_s(N, F, S0) ->
    {Value, S1} = F(S0),
    counts_incr(Value),
    true = Value =< 10,
    run_s(N - 1, F, S1).

test(N) ->
    <<I1:32/unsigned-integer,
      I2:32/unsigned-integer,
      I3:32/unsigned-integer,
      I4:32/unsigned-integer>> = crypto:strong_rand_bytes(16),
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    IP4 = I4 + 1,
    %counts_init(),
    %{Test1, _} = timer:tc(?MODULE, run, [N, fun test_now/0]),
    %counts_print("erlang:now/0"),
    counts_init(),
    {Test2, _} = timer:tc(?MODULE, run, [N, fun test_crypto/0]),
    counts_print("crypto:rand_uniform/2"),
    random:seed(IP1, IP2, IP3),
    counts_init(),
    {Test3, _} = timer:tc(?MODULE, run, [N, fun test_random/0]),
    counts_print("random:uniform/1"),
    % not uniform
    %counts_init(),
    %{Test4, _} = timer:tc(?MODULE, run, [N, fun test_reductions1/0]),
    %counts_print("erlang:process_info(self(), reductions)"),
    % not uniform
    %counts_init(),
    %{Test5, _} = timer:tc(?MODULE, run, [N, fun test_reductions2/0]),
    %counts_print("erlang:statistics(reductions)"),
    % not random
    %counts_init(),
    %{Test6, _} = timer:tc(?MODULE, run, [N, fun test_stats_io/0]),
    %counts_print("erlang:statistics(io)"),
    random_wh06_int:seed(IP1, IP2, IP3, IP4),
    counts_init(),
    {Test7, _} = timer:tc(?MODULE, run, [N, fun test_random_wh06_int/0]),
    counts_print("random_wh06_int:uniform/1"),
    counts_init(),
    {Test8, _} = timer:tc(?MODULE, run, [N, fun test_timestamp/0]),
    counts_print("os:timestamp/0"),
    % super slow
    %counts_init(),
    %{Test9, _} = timer:tc(?MODULE, run, [N, fun test_garbage_collections/0]),
    %counts_print("erlang:statistics(garbage_collection)"),
    % not uniform
    %counts_init(),
    %{Test10, _} = timer:tc(?MODULE, run, [N, fun test_context_switches/0]),
    %counts_print("erlang:statistics(context_switches)"),
    % not uniform, but quickest
    %counts_init(),
    %{Test11, _} = timer:tc(?MODULE, run, [N, fun test_make_ref/0]),
    %counts_print("erlang:make_ref/0"),
    counts_init(),
    {Test12, _} = timer:tc(?MODULE, run, [N, fun test_18_bxor_abs/0]),
    counts_print("18_bxor_abs"),
    counts_init(),
    {Test13, _} = timer:tc(?MODULE, run, [N, fun test_18_monotonic/0]),
    counts_print("18_monotonic"),
    %counts_init(),
    %{Test14, _} = timer:tc(?MODULE, run, [N, fun test_18_unique/0]),
    %counts_print("18_unique"),
    _ = rand:seed(exsplus, {IP1, IP2, IP3}),
    counts_init(),
    {Test15, _} = timer:tc(?MODULE, run, [N, fun test_18_rand/0]),
    counts_print("18_rand_exsplus"),
    _ = rand:seed(exs64, {IP1, IP2, IP3}),
    counts_init(),
    {Test16, _} = timer:tc(?MODULE, run, [N, fun test_18_rand/0]),
    counts_print("18_rand_exs64"),
    _ = rand:seed(exs1024, {IP1, IP2, IP3}),
    counts_init(),
    {Test17, _} = timer:tc(?MODULE, run, [N, fun test_18_rand/0]),
    counts_print("18_rand_exs1024"),
    random_wh82:seed(IP1, IP2, IP3),
    counts_init(),
    {Test18, _} = timer:tc(?MODULE, run, [N, fun test_random_wh82/0]),
    counts_print("random_wh82:uniform/1"),
    random_wh82_int:seed(IP1, IP2, IP3),
    counts_init(),
    {Test19, _} = timer:tc(?MODULE, run, [N, fun test_random_wh82_int/0]),
    counts_print("random_wh82_int:uniform/1"),
    counts_init(),
    {Test20, _} = timer:tc(?MODULE, run, [N, fun test_18_erlang_timestamp/0]),
    counts_print("18_erlang:system_time(micro_seconds)"),
    counts_init(),
    {Test21, _} = timer:tc(?MODULE, run, [N, fun test_19_perf_counter/0]),
    counts_print("19_os:perf_counter(micro_seconds)"),
    counts_init(),
    {Test22, _} = timer:tc(?MODULE, run, [N, fun test_os_time_perf_counter/0]),
    counts_print("os_time:perf_counter()"),
    counts_init(),
    {Test23, _} = timer:tc(?MODULE, run, [N, fun test_18_os_system_time/0]),
    counts_print("18_os:system_time(micro_seconds)"),
    {ok, _} = application:ensure_all_started(quickrand),
    ok = quickrand_cache:init(),
    counts_init(),
    {Test24, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_cache_uniform/0]),
    counts_print("quickrand_cache:uniform/1"),
    counts_init(),
    {Test25, _} = timer:tc(?MODULE, run_s, [N, fun test_quickrand_cache_uniform/1, quickrand_cache:new()]),
    counts_print("quickrand_cache:uniform/2"),

    %% results
    [
        %#result{name = "erlang:now/0",               get =  Test1},
        #result{name = "crypto:rand_uniform/2",      get =  Test2},
        #result{name = "random:uniform/1",           get =  Test3},
        %#result{name = "erlang:process_info(,red)",  get =  Test4},
        %#result{name = "erlang:statistics(red)",     get =  Test5},
        %#result{name = "erlang:statistics(io)",      get =  Test6},
        #result{name = "random_wh06_int:uniform/1",  get =  Test7},
        #result{name = "os:timestamp/0",             get =  Test8},
        %#result{name = "erlang:statistics(gc)",      get =  Test9},
        %#result{name = "erlang:statistics(cs)",      get =  Test10},
        %#result{name = "erlang:make_ref/0 hash",     get =  Test11}
        #result{name = "18_bxor_abs",                get =  Test12},
        #result{name = "18_monotonic",               get =  Test13},
        %#result{name = "18_unique",                  get =  Test14}%,
        #result{name = "18_rand_exsplus",            get =  Test15},
        #result{name = "18_rand_exs64",              get =  Test16},
        #result{name = "18_rand_exs1024",            get =  Test17},
        #result{name = "random_wh82:uniform/1",      get =  Test18},
        #result{name = "random_wh82_int:uniform/1",  get =  Test19},
        #result{name = "18_erlang:system_time/1",    get =  Test20},
        #result{name = "19_os:perf_counter/1",       get =  Test21},
        #result{name = "os_time:perf_counter/0",     get =  Test22},
        #result{name = "18_os:system_time/1",        get =  Test23},
        #result{name = "quickrand_cache:uniform/1",  get =  Test24},
        #result{name = "quickrand_cache:uniform/2",  get =  Test25}%,
    ].

