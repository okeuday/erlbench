%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Simple Hash Table Implementation.==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2011-2017 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(hasht).
-author('mjtruog at protonmail dot com').

%% external interface
-export([new/0,
         new/1,
         fetch/2,
         find/2,
         store/3]).

-define(PRIMES, [509, 1021, 2053, 4093, 8191, 16381, 32771, 65521]).

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

