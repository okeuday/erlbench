%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Simple Hash Table Implementation.==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(hasht).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         fetch/2,
         find/2,
         store/3]).

-define(PRIMES, [509, 509, 1021, 2053, 4093, 8191, 16381, 32771, 65521]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new() ->
    [Size | _] = ?PRIMES,
    {Size, erlang:make_tuple(Size, [])}.

new(Entries)
    when is_number(Entries) ->
    Size = case lists:dropwhile(fun(I) -> I < Entries end, ?PRIMES) of
        [H | _] ->
            H;
        [] ->
            lists:last(?PRIMES)
    end,
    {Size, erlang:make_tuple(Size, [])}.

store(Key, Value, {Size, Bins}) ->
    I = erlang:phash2(Key, Size) + 1,
    {Size, erlang:setelement(I, Bins, [{Key, Value} |
                                       erlang:element(I, Bins)])}.

fetch(Key, {Size, Bins}) ->
    I = erlang:phash2(Key, Size) + 1,
    case erlang:element(I, Bins) of
        [{Key, Value} | _] ->
            Value;
        [_, {Key, Value} | _] ->
            Value;
        L ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end.

find(Key, {Size, Bins}) ->
    I = erlang:phash2(Key, Size) + 1,
    case erlang:element(I, Bins) of
        [] ->
            error;
        [{Key, Value} | _] ->
            {ok, Value};
        [_, {Key, Value} | _] ->
            {ok, Value};
        L ->
            case lists:keyfind(Key, 1, L) of
                error ->
                    error;
                {Key, Value} ->
                    {ok, Value}
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

