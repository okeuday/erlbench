%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(uuid_creation).

-export([test/1, run/3, run/2]).

-include("erlbench.hrl").

run(1, F) ->
    F();
run(N, F) ->
    F(),
    run(N - 1, F).

run(1, F, A) ->
    F(A);
run(N, F, A) ->
    F(A),
    run(N - 1, F, A).

get_v4_strong() ->
    uuid:get_v4(strong).

get_v4_weak() ->
    uuid:get_v4(weak).

get_ref() ->
    erlang:make_ref().

test(N) ->
    application:start(crypto),

    StateErlang = uuid:new(self(), erlang),
    StateOs = uuid:new(self(), os),
    String = erlang:list_to_binary(string:chars($X, 64)),
    {V1a, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v1/1, StateErlang]),
    {V1b, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v1/1, StateOs]),
    {V3a, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v3/1, String]),
    {V3b, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v3_compat/1, String]),
    {V4a, _} = timer:tc(?MODULE, run, [N, fun get_v4_strong/0]),
    {V4b, _} = timer:tc(?MODULE, run, [N, fun get_v4_weak/0]),
    {V4c, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v4_urandom/0]),
    {V4d, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v4_urandom_bigint/0]),
    {V4e, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v4_urandom_native/0]),
    {V5a, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v5/1, String]),
    {V5b, _} = timer:tc(?MODULE, run, [N, fun uuid:get_v5_compat/1, String]),
    {V6, _} = timer:tc(?MODULE, run, [N, fun get_ref/0]),

    %% results
    [
        #result{name = "v1 erlang",                 get = V1a},
        #result{name = "v1 os",                     get = V1b},
        #result{name = "v3",                        get = V3a},
        #result{name = "v3 compat",                 get = V3b},
        #result{name = "v4 crypto strong",          get = V4a},
        #result{name = "v4 crypto weak",            get = V4b},
        #result{name = "v4 random_wh06_int",        get = V4c},
        #result{name = "v4 random bigint",          get = V4d},
        #result{name = "v4 random native",          get = V4e},
        #result{name = "v5",                        get = V5a},
        #result{name = "v5 compat",                 get = V5b},
        #result{name = "erlang:make_ref/0",         get = V6}
    ].

