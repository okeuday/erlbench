%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Binary tree data structure.==
%%% A binary tree data structure, with 7 entries per-node.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(btree7).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([find/2,
         new/0,
         store/3,
         test/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value.===
%% @end
%%-------------------------------------------------------------------------
find(_, {_, _, _, _, _, _, '$empty', _, _, _, _, _, _, _}) -> 
    error;
find(K, {_, _, _, _, _, _, K, V4, _, _, _, _, _, _}) -> 
    {ok, V4};
find(K, {K1, V1, K2, V2, K3, V3, K4, _, _, _, _, _, _, _})
    when K4 > K ->
    if
        K2 =:= '$empty' ->
            error;
        K2 == K ->
            {ok, V2};
        K2 > K ->
            if
                K1 =:= '$empty' ->
                    error;
                K1 =:= '$nested' ->
                    find(K, V1);
                K1 == K ->
                    {ok, V1};
                true ->
                    error
            end;
        K2 < K ->
            if
                K3 =:= '$empty' ->
                    error;
                K3 =:= '$nested' ->
                    find(K, V3);
                K3 == K ->
                    {ok, V3};
                true ->
                    error
            end
    end;
find(K, {_, _, _, _, _, _, K4, _, K5, V5, K6, V6, K7, V7})
    when K4 < K ->
    if
        K6 =:= '$empty' ->
            error;
        K6 == K ->
            {ok, V6};
        K6 > K ->
            if
                K5 =:= '$empty' ->
                    error;
                K5 =:= '$nested' ->
                    find(K, V5);
                K5 == K ->
                    {ok, V5};
                true ->
                    error
            end;
        K6 < K ->
            if
                K7 =:= '$empty' ->
                    error;
                K7 =:= '$nested' ->
                    find(K, V7);
                K7 == K ->
                    {ok, V7};
                true ->
                    error
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new instance.===
%% @end
%%-------------------------------------------------------------------------

new() ->
    {'$empty', '$empty', '$empty', '$empty', '$empty', '$empty', '$empty',
     '$empty', '$empty', '$empty', '$empty', '$empty', '$empty', '$empty'}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key-value pair.===
%% @end
%%-------------------------------------------------------------------------

store(K, V, {K1, V1, K2, V2, K3, V3, '$empty', _, K5, V5, K6, V6, K7, V7}) ->
    {K1, V1, K2, V2, K3, V3, K, V, K5, V5, K6, V6, K7, V7};
store(K, V, {K1, V1, K2, V2, K3, V3, K4, _, K5, V5, K6, V6, K7, V7})
    when K4 == K ->
    {K1, V1, K2, V2, K3, V3, K, V, K5, V5, K6, V6, K7, V7};
store(K, V, {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7})
    when K4 > K ->
    if
        K2 =:= '$empty'; K2 == K ->
            {K1, V1, K, V, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7};
        K2 > K ->
            if
                K1 =:= '$empty' ->
                    {K, V, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7};
                K1 =:= '$nested' ->
                    {K1, store(K, V, V1),
                     K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7};
                true ->
                    {'$nested',
                     store(K, V,
                           {'$empty', '$empty', '$empty', '$empty',
                            '$empty', '$empty', K1,
                            V1, '$empty', '$empty', '$empty',
                            '$empty', '$empty', '$empty'}),
                     K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7}
            end;
        K2 < K ->
            if
                K3 =:= '$empty' ->
                    {K1, V1, K2, V2, K, V, K4, V4, K5, V5, K6, V6, K7, V7};
                K3 =:= '$nested' ->
                    {K1, V1, K2, V2, K3, store(K, V, V3),
                     K4, V4, K5, V5, K6, V6, K7, V7};
                true ->
                    {K1, V1, K2, V2,
                     '$nested',
                     store(K, V,
                           {'$empty', '$empty', '$empty', '$empty',
                            '$empty', '$empty', K3,
                            V3, '$empty', '$empty', '$empty',
                            '$empty', '$empty', '$empty'}),
                     K4, V4, K5, V5, K6, V6, K7, V7}
            end
    end;
store(K, V, {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7, V7})
    when K4 < K ->
    if
        K6 =:= '$empty'; K6 == K ->
            {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K, V, K7, V7};
        K6 > K ->
            if
                K5 =:= '$empty' ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, K, V, K6, V6, K7, V7};
                K5 =:= '$nested' ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, K5,
                     store(K, V, V5), K6, V6, K7, V7};
                true ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, 
                     '$nested',
                     store(K, V,
                           {'$empty', '$empty', '$empty', '$empty',
                            '$empty', '$empty', K5,
                            V5, '$empty', '$empty', '$empty',
                            '$empty', '$empty', '$empty'}),
                     K6, V6, K7, V7}
            end;
        K6 < K ->
            if
                K7 =:= '$empty' ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K, V};
                K7 =:= '$nested' ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6, K7,
                     store(K, V, V7)};
                true ->
                    {K1, V1, K2, V2, K3, V3, K4, V4, K5, V5, K6, V6,
                     '$nested',
                     store(K, V,
                           {'$empty', '$empty', '$empty', '$empty',
                            '$empty', '$empty', K7,
                            V7, '$empty', '$empty', '$empty',
                            '$empty', '$empty', '$empty'})}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Internal test.===
%% @end
%%-------------------------------------------------------------------------
test() ->
    ok.

