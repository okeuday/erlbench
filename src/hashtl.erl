%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Hash Table Layered Implementation.==
%%% Simple statically sized hash table implementation.  Layers of tuples are
%%% used for efficient assignment operations with complex keys (strings, etc.).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011-2017 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(hashtl).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([delete/2,
         fetch/2,
         find/2,
         new/0,
         new/1,
         store/3]).

-define(PRIMES, [127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

delete(Key, {Size, Bins}) ->
    {Size, delete_key(hash_key(Size, Key), Key, Bins)}.

fetch(Key, {Size, Bins}) ->
    fetch_key(hash_key(Size, Key), Bins, Key).

find(Key, {Size, Bins}) ->
    find_key(hash_key(Size, Key), Bins, Key).

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

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create([]) ->
    [];

create([I | Is]) ->
    erlang:make_tuple(I + 1, create(Is)).

delete_key({I1}, Key, Bins1) ->
    erlang:setelement(I1, Bins1,
                      lists:keydelete(Key, 1, erlang:element(I1, Bins1)));

delete_key({I1, I2}, Key, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
                      lists:keydelete(Key, 1, erlang:element(I2, Bins2))));

delete_key({I1, I2, I3}, Key, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3,
                      lists:keydelete(Key, 1, erlang:element(I3, Bins3)))));

delete_key({I1, I2, I3, I4}, Key, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    Bins4 = erlang:element(I3, Bins3),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3,
    erlang:setelement(I4, Bins4,
                      lists:keydelete(Key, 1, erlang:element(I4, Bins4)))))).

fetch_key({I1}, Bins1, Key) ->
    [H | _] = L = erlang:element(I1, Bins1),
    if
        erlang:element(1, H) == Key ->
            erlang:element(2, H);
        true ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end;

fetch_key({I1, I2}, Bins1, Key) ->
    [H | _] = L = erlang:element(I2,
                  erlang:element(I1, Bins1)),
    if
        erlang:element(1, H) == Key ->
            erlang:element(2, H);
        true ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end;

fetch_key({I1, I2, I3}, Bins1, Key) ->
    [H | _] = L = erlang:element(I3,
                  erlang:element(I2,
                  erlang:element(I1, Bins1))),
    if
        erlang:element(1, H) == Key ->
            erlang:element(2, H);
        true ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end;

fetch_key({I1, I2, I3, I4}, Bins1, Key) ->
    [H | _] = L = erlang:element(I4,
                  erlang:element(I3,
                  erlang:element(I2,
                  erlang:element(I1, Bins1)))),
    if
        erlang:element(1, H) == Key ->
            erlang:element(2, H);
        true ->
            {Key, Value} = lists:keyfind(Key, 1, L),
            Value
    end.

find_key({I1}, Bins1, Key) ->
    case erlang:element(I1, Bins1) of
        [] ->
            error;
        [{Key, Value} | _] ->
            {ok, Value};
        L ->
            case lists:keyfind(Key, 1, L) of
                error ->
                    error;
                {Key, Value} ->
                    {ok, Value}
            end
    end;

find_key({I1, I2}, Bins1, Key) ->
    case erlang:element(I2,
         erlang:element(I1, Bins1)) of
        [] ->
            error;
        [{Key, Value} | _] ->
            {ok, Value};
        L ->
            case lists:keyfind(Key, 1, L) of
                error ->
                    error;
                {Key, Value} ->
                    {ok, Value}
            end
    end;

find_key({I1, I2, I3}, Bins1, Key) ->
    case erlang:element(I3,
         erlang:element(I2,
         erlang:element(I1, Bins1))) of
        [] ->
            error;
        [{Key, Value} | _] ->
            {ok, Value};
        L ->
            case lists:keyfind(Key, 1, L) of
                error ->
                    error;
                {Key, Value} ->
                    {ok, Value}
            end
    end;

find_key({I1, I2, I3, I4}, Bins1, Key) ->
    case erlang:element(I4,
         erlang:element(I3,
         erlang:element(I2,
         erlang:element(I1, Bins1)))) of
        [] ->
            error;
        [{Key, Value} | _] ->
            {ok, Value};
        L ->
            case lists:keyfind(Key, 1, L) of
                error ->
                    error;
                {Key, Value} ->
                    {ok, Value}
            end
    end.

store_key({I1}, KV, Bins1) ->
    erlang:setelement(I1, Bins1, [KV | erlang:element(I1, Bins1)]);

store_key({I1, I2}, KV, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2, [KV | erlang:element(I2, Bins2)]));

store_key({I1, I2, I3}, KV, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3, [KV | erlang:element(I3, Bins3)])));

store_key({I1, I2, I3, I4}, KV, Bins1) ->
    Bins2 = erlang:element(I1, Bins1),
    Bins3 = erlang:element(I2, Bins2),
    Bins4 = erlang:element(I3, Bins3),
    erlang:setelement(I1, Bins1,
    erlang:setelement(I2, Bins2,
    erlang:setelement(I3, Bins3,
    erlang:setelement(I4, Bins4, [KV | erlang:element(I4, Bins4)])))).

-compile({inline, [{mod,2},{hash,2},{hash_key,2},{hash_max,1}]}).

mod(X, Y)
    when X > 0 ->
    X rem Y;
mod(X, Y)
    when X < 0 ->
    Y + X rem Y;
mod(0, _) ->
    0.

hash(Key, Value)
    when is_integer(Key) ->
    mod(Key, Value);
hash(Key, Value) ->
    erlang:phash2(Key, Value).

hash_key(127, Key) ->
    {hash(Key, 127) + 1};

hash_key(251, Key) ->
    Hash = hash(Key, 251),
    <<I1:4, I2:4>> = <<Hash:8>>,
    {I1 + 1, I2 + 1};

hash_key(509, Key) ->
    Hash = hash(Key, 509),
    <<I1:4, I2:5>> = <<Hash:9>>,
    {I1 + 1, I2 + 1};

hash_key(1021, Key) ->
    Hash = hash(Key, 1021),
    <<I1:3, I2:3, I3:4>> = <<Hash:10>>,
    {I1 + 1, I2 + 1, I3 + 1};

hash_key(2039, Key) ->
    Hash = hash(Key, 2039),
    <<I1:3, I2:4, I3:4>> = <<Hash:11>>,
    {I1 + 1, I2 + 1, I3 + 1};

hash_key(4093, Key) ->
    Hash = hash(Key, 4093),
    <<I1:4, I2:4, I3:4>> = <<Hash:12>>,
    {I1 + 1, I2 + 1, I3 + 1};

hash_key(8191, Key) ->
    Hash = hash(Key, 8191),
    <<I1:4, I2:4, I3:5>> = <<Hash:13>>,
    {I1 + 1, I2 + 1, I3 + 1};

hash_key(16381, Key) ->
    Hash = hash(Key, 16381),
    <<I1:4, I2:5, I3:5>> = <<Hash:14>>,
    {I1 + 1, I2 + 1, I3 + 1};

hash_key(32749, Key) ->
    Hash = hash(Key, 32749),
    <<I1:3, I2:4, I3:4, I4:4>> = <<Hash:15>>,
    {I1 + 1, I2 + 1, I3 + 1, I4 + 1};

hash_key(65521, Key) ->
    Hash = hash(Key, 65521),
    <<I1:4, I2:4, I3:4, I4:4>> = <<Hash:16>>,
    {I1 + 1, I2 + 1, I3 + 1, I4 + 1}.

hash_max(127) ->
    <<I1:7>> = <<127:7>>,
    [I1];

hash_max(251) ->
    <<I1:4, I2:4>> = <<255:8>>,
    [I1, I2];

hash_max(509) ->
    <<I1:4, I2:5>> = <<511:9>>,
    [I1, I2];

hash_max(1021) ->
    <<I1:3, I2:3, I3:4>> = <<1023:10>>,
    [I1, I2, I3];

hash_max(2039) ->
    <<I1:3, I2:4, I3:4>> = <<2047:11>>,
    [I1, I2, I3];

hash_max(4093) ->
    <<I1:4, I2:4, I3:4>> = <<4095:12>>,
    [I1, I2, I3];

hash_max(8191) ->
    <<I1:4, I2:4, I3:5>> = <<8191:13>>,
    [I1, I2, I3];

hash_max(16381) ->
    <<I1:4, I2:5, I3:5>> = <<16383:14>>,
    [I1, I2, I3];

hash_max(32749) ->
    <<I1:3, I2:4, I3:4, I4:4>> = <<32767:15>>,
    [I1, I2, I3, I4];

hash_max(65521) ->
    <<I1:4, I2:4, I3:4, I4:4>> = <<65535:16>>,
    [I1, I2, I3, I4].

