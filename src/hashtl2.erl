%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Hash Table Layered Implementation.==
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

-module(hashtl2).
-author('mjtruog at protonmail dot com').

%% external interface
-export([new/0,
         new/1,
         fetch/2,
         find/2,
         store/3]).

-define(PRIMES, [509, 1021, 2039, 4093, 8191, 16381, 32749, 65521]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

new() ->
    [Size | _] = ?PRIMES,
    {Size, create(hash_max(Size))}.

new(Entries)
    when is_number(Entries) ->
    Size = case lists:dropwhile(fun(I) -> I < Entries end, ?PRIMES) of
        [H | _] ->
            H;
        [] ->
            lists:last(?PRIMES)
    end,
    {Size, create(hash_max(Size))}.

store(Key, Value, {Size, Bins}) ->
    {Size, store_key(hash_key(Size, Key), {Key, Value}, Bins)}.

fetch(Key, {Size, Bins}) ->
    fetch_key(hash_key(Size, Key), Bins, Key).

find(Key, {Size, Bins}) ->
    find_key(hash_key(Size, Key), Bins, Key).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create([]) ->
    [];

create([I | Is]) ->
    erlang:make_tuple(I + 1, create(Is)).

store_key([I], KV, Bins) ->
    erlang:setelement(I, Bins, [KV | erlang:element(I, Bins)]);

store_key([I | Is], KV, Bins) ->
    erlang:setelement(I, Bins, store_key(Is, KV, erlang:element(I, Bins))).

fetch_key([I], Bins, Key) ->
    case erlang:element(I, Bins) of
        [{Key, Value} | _] ->
            Value;
        [_, {Key, Value} | _] ->
            Value;
        L ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end;

fetch_key([I | Is], Bins, Key) ->
    fetch_key(Is, erlang:element(I, Bins), Key).

find_key([I], Bins, Key) ->
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
    end;

find_key([I | Is], Bins, Key) ->
    find_key(Is, erlang:element(I, Bins), Key).

hash_key(509, Key) ->
    Hash = erlang:phash2(Key, 509),
    <<I1:4, I2:5>> = <<Hash:9>>,
    [I1 + 1, I2 + 1];

hash_key(1021, Key) ->
    Hash = erlang:phash2(Key, 1021),
    <<I1:5, I2:5>> = <<Hash:10>>,
    [I1 + 1, I2 + 1];

hash_key(2039, Key) ->
    Hash = erlang:phash2(Key, 2039),
    <<I1:5, I2:6>> = <<Hash:11>>,
    [I1 + 1, I2 + 1];

hash_key(4093, Key) ->
    Hash = erlang:phash2(Key, 4093),
    <<I1:6, I2:6>> = <<Hash:12>>,
    [I1 + 1, I2 + 1];

hash_key(8191, Key) ->
    Hash = erlang:phash2(Key, 8191),
    <<I1:6, I2:7>> = <<Hash:13>>,
    [I1 + 1, I2 + 1];

hash_key(16381, Key) ->
    Hash = erlang:phash2(Key, 16381),
    <<I1:7, I2:7>> = <<Hash:14>>,
    [I1 + 1, I2 + 1];

hash_key(32749, Key) ->
    Hash = erlang:phash2(Key, 32749),
    <<I1:7, I2:8>> = <<Hash:15>>,
    [I1 + 1, I2 + 1];

hash_key(65521, Key) ->
    Hash = erlang:phash2(Key, 65521),
    <<I1:8, I2:8>> = <<Hash:16>>,
    [I1 + 1, I2 + 1].

hash_max(509) ->
    <<I1:4, I2:5>> = <<511:9>>,
    [I1, I2];

hash_max(1021) ->
    <<I1:5, I2:5>> = <<1023:10>>,
    [I1, I2];

hash_max(2039) ->
    <<I1:5, I2:6>> = <<2047:11>>,
    [I1, I2];

hash_max(4093) ->
    <<I1:6, I2:6>> = <<4095:12>>,
    [I1, I2];

hash_max(8191) ->
    <<I1:6, I2:7>> = <<8191:13>>,
    [I1, I2];

hash_max(16381) ->
    <<I1:7, I2:7>> = <<16383:14>>,
    [I1, I2];

hash_max(32749) ->
    <<I1:7, I2:8>> = <<32767:15>>,
    [I1, I2];

hash_max(65521) ->
    <<I1:8, I2:8>> = <<65535:16>>,
    [I1, I2].

