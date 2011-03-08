%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Hash Table Layered Implementation.==
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

-module(hashtl).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         fetch/2,
         find/2,
         store/3]).

-define(PRIMES, [127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521]).

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

