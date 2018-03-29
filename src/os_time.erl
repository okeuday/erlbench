%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2016-2017 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%%------------------------------------------------------------------------

-module(os_time).
-author('mjtruog at protonmail dot com').

%% external interface
-export([perf_counter/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-on_load(init/0).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

init() ->
    Path = "priv",
    erlang:load_nif(filename:join([Path, "libos_time"]), []).

-spec perf_counter() ->
    non_neg_integer().

perf_counter() ->
    {ok, Seconds, NanoSeconds} = perf_counter_value(),
    1000000000 * Seconds + NanoSeconds.

-spec perf_counter_value() ->
    {ok, non_neg_integer(), non_neg_integer()} | {error, atom()}.

perf_counter_value() ->
    erlang:nif_error(not_loaded).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

