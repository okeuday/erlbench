%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(pseudo_randomness).

-export([test/1,
         run/2, run_s/3,
         run_nonuniform/2, run_nonuniform_s/3,
         run_nonuniform2/2, run_nonuniform2_s/3]).

-include("erlbench.hrl").

% updating the distribution skews the results
%-define(PRINT_DISTRIBUTION, true).

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

test_18_phash2_unique() ->
    % not uniform at all, excluded from test
    erlang:phash2(erlang:unique_integer(), 10) + 1.

test_20_rand() ->
    rand:uniform(10).

test_20_rand_normal() ->
    rand:normal(0, 1).

%test_now() ->
%    % most uniform solution
%    {_, _, I} = erlang:now(),
%    (I rem 10) + 1.

%test_crypto() ->
%    crypto:rand_uniform(1, 11).

%test_random() ->
%    random:uniform(10).

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

test_22_os_time_perf_counter() ->
    I = os_time:perf_counter(),
    (I rem 10) + 1.

test_quickrand_cache_uniform() ->
    quickrand_cache:uniform(10).

test_quickrand_cache_uniform(State) ->
    quickrand_cache:uniform(10, State).

test_quickrand_cache_float() ->
    quickrand_cache:floatR().

test_quickrand_cache_float(State) ->
    quickrand_cache:floatR(State).

test_quickrand_cache_normal_box_muller() ->
    quickrand_cache_normal:box_muller(0, 1).

test_quickrand_cache_normal_box_muller(State) ->
    quickrand_cache_normal:box_muller(0, 1, State).

test_quickrand_strong_uniform() ->
    quickrand:strong_uniform(10).

test_quickrand_lcg35() ->
    quickrand:lcg35(10).

test_quickrand_mcg35() ->
    quickrand:mcg35(10).

test_quickrand_mwc128_64() ->
    quickrand:mwc128_64(10).

test_quickrand_mwc256_64() ->
    quickrand:mwc256_64(10).

test_quickrand_mwc256_128() ->
    quickrand:mwc256_128(10).

-compile({inline,
          [{counts_incr,1}]}).

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

run_nonuniform(0, _) ->
    ok;
run_nonuniform(N, F) ->
    Value = F(),
    true = (Value >= -10) andalso (Value =< 10),
    run_nonuniform(N - 1, F).

run_nonuniform_s(0, _, _) ->
    ok;
run_nonuniform_s(N, F, S0) ->
    {Value, S1} = F(S0),
    true = (Value >= -10) andalso (Value =< 10),
    run_nonuniform_s(N - 1, F, S1).

run_nonuniform2(-1, _) ->
    ok;
run_nonuniform2(0, _) ->
    ok;
run_nonuniform2(N, F) ->
    {Value1, Value2} = F(),
    true = (Value1 >= -10) andalso (Value1 =< 10),
    true = (Value2 >= -10) andalso (Value2 =< 10),
    run_nonuniform2(N - 2, F).

run_nonuniform2_s(-1, _, _) ->
    ok;
run_nonuniform2_s(0, _, _) ->
    ok;
run_nonuniform2_s(N, F, S0) ->
    {Value1, Value2, S1} = F(S0),
    true = (Value1 >= -10) andalso (Value1 =< 10),
    true = (Value2 >= -10) andalso (Value2 =< 10),
    run_nonuniform2_s(N - 2, F, S1).

test(N) ->
    ok = quickrand:seed(),
    <<I1:64/unsigned-integer,
      I2:64/unsigned-integer,
      I3:64/unsigned-integer>> = crypto:strong_rand_bytes(24),
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    %counts_init(),
    %{Test1, _} = timer:tc(?MODULE, run, [N, fun test_now/0]),
    %counts_print("erlang:now/0"),
    %counts_init(),
    %{Test2, _} = timer:tc(?MODULE, run, [N, fun test_crypto/0]),
    %counts_print("crypto:rand_uniform/2"),
    %counts_init(),
    %{Test3, _} = timer:tc(?MODULE, run, [N, fun test_random/0]),
    %counts_print("random:uniform/1"),
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
    %_ = rand:seed(exsplus, {IP1, IP2, IP3}),
    _ = rand:seed(exsp, {IP1, IP2, IP3}),
    counts_init(),
    {Test15, _} = timer:tc(?MODULE, run, [N, fun test_20_rand/0]),
    counts_print("20_rand_exsp"),
    %_ = rand:seed(exs64, {IP1, IP2, IP3}),
    _ = rand:seed(exrop, {IP1, IP2, IP3}),
    counts_init(),
    {Test16, _} = timer:tc(?MODULE, run, [N, fun test_20_rand/0]),
    counts_print("20_rand_exrop"),
    %_ = rand:seed(exs1024, {IP1, IP2, IP3}),
    _ = rand:seed(exs1024s, {IP1, IP2, IP3}),
    counts_init(),
    {Test17, _} = timer:tc(?MODULE, run, [N, fun test_20_rand/0]),
    counts_print("20_rand_exs1024s"),
    counts_init(),
    {Test18, _} = timer:tc(?MODULE, run, [N, fun test_random_wh82/0]),
    counts_print("random_wh82:uniform/1"),
    counts_init(),
    {Test19, _} = timer:tc(?MODULE, run, [N, fun test_random_wh82_int/0]),
    counts_print("random_wh82_int:uniform/1"),
    counts_init(),
    {Test20, _} = timer:tc(?MODULE, run, [N, fun test_18_erlang_timestamp/0]),
    counts_print("18_erlang:system_time(micro_seconds)"),
    counts_init(),
    {Test21, _} = timer:tc(?MODULE, run, [N, fun test_19_perf_counter/0]),
    counts_print("19_os:perf_counter(micro_seconds)"),
    %counts_init(),
    %{Test22, _} = timer:tc(?MODULE, run, [N, fun test_22_os_time_perf_counter/0]),
    %counts_print("22_os_time:perf_counter()"),
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

    {Test26, _} = timer:tc(?MODULE, run_nonuniform, [N, fun test_quickrand_cache_float/0]),
    {Test27, _} = timer:tc(?MODULE, run_nonuniform_s, [N, fun test_quickrand_cache_float/1, quickrand_cache:new()]),

    % normal distribution randomness, not a uniform distribution
    _ = rand:seed(exsp, {IP1, IP2, IP3}),
    {Test28, _} = timer:tc(?MODULE, run_nonuniform, [N, fun test_20_rand_normal/0]),
    {Test29, _} = timer:tc(?MODULE, run_nonuniform2, [N, fun test_quickrand_cache_normal_box_muller/0]),
    {Test30, _} = timer:tc(?MODULE, run_nonuniform2_s, [N, fun test_quickrand_cache_normal_box_muller/1, quickrand_cache:new()]),
    counts_init(),
    {Test31, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_lcg35/0]),
    counts_print("quickrand:lcg35/1"),
    counts_init(),
    {Test32, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_mcg35/0]),
    counts_print("quickrand:mcg35/1"),
    %{Test33, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_strong_uniform/0]),
    %counts_init(),
    %{Test34, _} = timer:tc(?MODULE, run, [N, fun test_18_phash2_unique/0]),
    %counts_print("18_unique_phash2"),
    counts_init(),
    {Test35, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_mwc128_64/0]),
    counts_print("quickrand:mwc128_64/1"),
    counts_init(),
    {Test36, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_mwc256_64/0]),
    counts_print("quickrand:mwc256_64/1"),
    counts_init(),
    {Test37, _} = timer:tc(?MODULE, run, [N, fun test_quickrand_mwc256_128/0]),
    counts_print("quickrand:mwc256_128/1"),

    %% results
    [
        %#result{name = "erlang:now/0",               get =  Test1},
        %#result{name = "crypto:rand_uniform/2",      get =  Test2},
        %#result{name = "random:uniform/1",           get =  Test3},
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
        %#result{name = "18_unique",                  get =  Test14},
        #result{name = "20_rand_exsp",               get =  Test15},
        #result{name = "20_rand_exrop",              get =  Test16},
        #result{name = "20_rand_exs1024s",           get =  Test17},
        #result{name = "random_wh82:uniform/1",      get =  Test18},
        #result{name = "random_wh82_int:uniform/1",  get =  Test19},
        #result{name = "18_erlang:system_time/1",    get =  Test20},
        #result{name = "19_os:perf_counter/1",       get =  Test21},
        %#result{name = "22_os_time:perf_counter/0",     get =  Test22},
        #result{name = "18_os:system_time/1",        get =  Test23},
        #result{name = "quickrand_c:uni/1",          get =  Test24},
        #result{name = "quickrand_c:uni/2",          get =  Test25},
        #result{name = "quickrand_c:floatR/0",       get =  Test26},
        #result{name = "quickrand_c:floatR/1",       get =  Test27},
        #result{name = "20_rand:normal",             get =  Test28},
        #result{name = "quickrand_c_normal/2",       get =  Test29},
        #result{name = "quickrand_c_normal/3",       get =  Test30},
        #result{name = "quickrand:lcg35/1",          get =  Test31},
        #result{name = "quickrand:mcg35/1",          get =  Test32},
        %#result{name = "quickrand:strong_uniform/1", get =  Test33}%,
        %#result{name = "18_unique_phash2",           get =  Test34}%,
        #result{name = "quickrand:mwc128_64/1",      get =  Test35},
        #result{name = "quickrand:mwc256_64/1",      get =  Test36},
        #result{name = "quickrand:mwc256_128/1",     get =  Test37}%,
    ].

