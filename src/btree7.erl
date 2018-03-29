%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Binary tree data structure.==
%%% A binary tree data structure, with 7 entries per-node.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2017 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(btree7).
-author('mjtruog at protonmail dot com').

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

