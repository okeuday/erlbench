%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

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

test(N) ->
    application:start(crypto),

    State = uuid:new(self()),
    String = string:chars($X, 64),
    {V1, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v1/1, State]),
    {V3, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v3/1, String]),
    {V4, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v4/0]),
    {V4a, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v4_urandom_bigint/0]),
    {V4b, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v4_urandom_native/0]),
    {V5, _} = timer:tc(uuid_creation, run, [N, fun uuid:get_v5/1, String]),

    %% results
    [
        #result{name = "v1 native",                        get = V1},
        #result{name = "v3 native",                        get = V3},
        #result{name = "v4 native",                        get = V4},
        #result{name = "v4 native urandom bigint",         get = V4a},
        #result{name = "v4 native urandom native",         get = V4b},
        #result{name = "v5 native",                        get = V5}
    ].

