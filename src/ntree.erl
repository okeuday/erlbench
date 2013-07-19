%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==N-tree data structure.==
%%% Attempting to exploit flat tree hierarchies with an adjustable size N.
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

-module(ntree).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([find/2,
         new/0,
         new/1,
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
find(K, {N, _, Elements}) when is_list(Elements) ->
    find_value_list(K, Elements, N);
find(K, {N, _, Elements}) when is_tuple(Elements) ->
    find_value_tuple(erlang:round(N / 2), 1, N,
                     K, Elements, N).

find_value_list(K, [{K, V} | _], _) ->
    {ok, V};
find_value_list(K, [{K1, _} | Elements], N) when K1 < K ->
    find_value_list(K, Elements, N);
find_value_list(K, [{KT1, KT2, _, ElementsNext} | _], N)
    when K >= KT1, K =< KT2 ->
    if
        is_list(ElementsNext) ->
            find_value_list(K, ElementsNext, N);
        is_tuple(ElementsNext) ->
            find_value_tuple(erlang:round(N / 2), 1, N, K, ElementsNext, N)
    end;
find_value_list(K, [{_, KT2, _, _} | Elements], N) when KT2 < K ->
    find_value_list(K, Elements, N);
find_value_list(_, _, _) ->
    error.

find_value_tuple(I, LeftI, RightI, K, Elements, N)
    when I =:= LeftI; I =:= RightI ->
    case erlang:element(I, Elements) of
        {K, V} ->
            {ok, V};
        {_, _} ->
            error;
        {KT1, KT2, _, _} when K < KT1; K > KT2 ->
            error;
        {_, _, _, ElementsNext} when is_list(ElementsNext) ->
            find_value_list(K, ElementsNext, N);
        {_, _, _, ElementsNext} when is_tuple(ElementsNext) ->
            find_value_tuple(erlang:round(N / 2), 1, N,
                             K, ElementsNext, N)
    end;
find_value_tuple(I, LeftI, RightI, K, Elements, N) ->
    case erlang:element(I, Elements) of
        {K, V} ->
            {ok, V};
        {K1, _} when K1 > K ->
            if
                LeftI =:= I - 1 ->
                    find_value_tuple(LeftI, LeftI, I, K, Elements, N);
                true ->
                    find_value_tuple(erlang:round((LeftI + I) / 2),
                                     LeftI, I, K, Elements, N)
            end;
        {_, _} -> % K1 < K
            if
                RightI =:= I + 1 ->
                    find_value_tuple(RightI, I, RightI, K, Elements, N);
                true ->
                    find_value_tuple(erlang:round((I + RightI) / 2),
                                     I, RightI, K, Elements, N)
            end;
        {KT1, _, _, _} when KT1 > K ->
            if
                LeftI =:= I - 1 ->
                    find_value_tuple(LeftI, LeftI, I, K, Elements, N);
                true ->
                    find_value_tuple(erlang:round((LeftI + I) / 2),
                                     LeftI, I, K, Elements, N)
            end;
        {_, KT2, _, _} when KT2 < K ->
            if
                RightI =:= I + 1 ->
                    find_value_tuple(RightI, I, RightI, K, Elements, N);
                true ->
                    find_value_tuple(erlang:round((I + RightI) / 2),
                                     I, RightI, K, Elements, N)
            end;
        {_, _, _, ElementsNext} when is_list(ElementsNext) ->
            find_value_list(K, ElementsNext, N);
        {_, _, _, ElementsNext} when is_tuple(ElementsNext) ->
            find_value_tuple(erlang:round(N / 2), 1, N,
                             K, ElementsNext, N)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new instance.===
%% tree:
%% {N, L_length, L_list}
%% each element of L:
%% {K, V} (or)
%% {K_low, K_high, L_length, L_list}
%% @end
%%-------------------------------------------------------------------------

new() ->
    new(10).

new(N) when is_integer(N), N >= 2 ->
    {N, 0, []}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key-value pair.===
%% @end
%%-------------------------------------------------------------------------

store(K, V, {N, N, Elements}) when is_list(Elements) ->
    {N, N,
     store_element_1_tuple(erlang:round(N / 2), 1, N,
                           erlang:list_to_tuple(Elements), K, V, N)};
store(K, V, {N, N, Elements}) when is_tuple(Elements) ->
    {N, N,
     store_element_1_tuple(erlang:round(N / 2), 1, N,
                           Elements, K, V, N)};
store(K, V, {N, 0, []}) ->
    {N, 1, [{K, V}]};
store(K, V, {N, ElementCount, Elements}) ->
    ElementsNew = store_element_0_list(Elements, K, V, N),
    ElementCountNew = if
        ElementCount =:= N - 1 ->
            erlang:length(ElementsNew);
        true ->
            ElementCount + 1
    end,
    {N, ElementCountNew, ElementsNew}.

store_element_0_list([], K1, V1, _) ->
    [{K1, V1}];
store_element_0_list([{K1, _} | Elements], K1, V1, _) ->
    [{K1, V1} | Elements];
store_element_0_list([{K2, _} = E | Elements], K1, V1, N) when K2 < K1 ->
    [E | store_element_0_list(Elements, K1, V1, N)];
store_element_0_list([{_, _} | _] = Elements, K1, V1, _) ->
    [{K1, V1} | Elements];
store_element_0_list([{_, KT2, _, _} = E |
                      Elements], K1, V1, N) when KT2 < K1 ->
    [E | store_element_0_list(Elements, K1, V1, N)];
store_element_0_list([{KT1, _, _, _} |
                      _] = Elements, K1, V1, _) when KT1 > K1 ->
    [{K1, V1} | Elements];
store_element_0_list([{KT1, KT2, N, ElementsNext} |
                      Elements], K1, V1, N) ->
    if
        is_list(ElementsNext) ->
            [{KT1, KT2, N,
              store_element_1_tuple(erlang:round(N / 2), 1, N,
                                    erlang:list_to_tuple(ElementsNext),
                                    K1, V1, N)} | Elements];
        is_tuple(ElementsNext) ->
            [{KT1, KT2, N,
              store_element_1_tuple(erlang:round(N / 2), 1, N,
                                    ElementsNext,
                                    K1, V1, N)} | Elements]
    end;
store_element_0_list([{KT1, KT2, ElementCountNext, ElementsNext} |
                      Elements], K1, V1, N) ->
    ElementsNextNew = store_element_0_list(ElementsNext, K1, V1, N),
    ElementCountNextNew = if
        ElementCountNext =:= N - 1 ->
            erlang:length(ElementsNextNew);
        true ->
            ElementCountNext + 1
    end,
    [{KT1, KT2, ElementCountNextNew, ElementsNextNew} | Elements].

store_element_1_list([{K1, _} | Elements], K1, V1, _) ->
    [{K1, V1} | Elements];
store_element_1_list([{K2, _} = E | Elements], K1, V1, N) when K2 < K1 ->
    if
        Elements =:= [] ->
            [{K2, K1, 2, [E, {K1, V1}]}];
        true ->
            [E | store_element_1_list(Elements, K1, V1, N)]
    end;
store_element_1_list([{K2, _} = E | Elements], K1, V1, _) ->
    [{K1, K2, 2, [{K1, V1}, E]} | Elements];
store_element_1_list([{KT1, KT2, ElementCountNext, ElementsNext} = E |
                 Elements], K1, V1, N) when KT2 < K1 ->
    if
        Elements =:= [] ->
            if
                ElementCountNext =:= N ->
                    [{KT1, K1, N,
                      store_element_1_list(ElementsNext, K1, V1, N)}];
                true ->
                    ElementsNextNew = store_element_0_list(ElementsNext,
                                                           K1, V1, N),
                    ElementCountNextNew = if
                        ElementCountNext =:= N - 1 ->
                            erlang:length(ElementsNextNew);
                        true ->
                            ElementCountNext + 1
                    end,
                    [{KT1, K1, ElementCountNextNew, ElementsNextNew}]
            end;
        true ->
            [E | store_element_1_list(Elements, K1, V1, N)]
    end;
store_element_1_list([{KT1, KT2, N, ElementsNext} |
                 Elements], K1, V1, N) ->
    [{erlang:min(KT1, K1), KT2, N,
      store_element_1_list(ElementsNext, K1, V1, N)} | Elements];
store_element_1_list([{KT1, KT2, ElementCountNext, ElementsNext} |
                 Elements], K1, V1, N) ->
    ElementsNextNew = store_element_0_list(ElementsNext, K1, V1, N),
    ElementCountNextNew = if
        ElementCountNext =:= N - 1 ->
            erlang:length(ElementsNextNew);
        true ->
            ElementCountNext + 1
    end,
    [{erlang:min(KT1, K1), KT2,
      ElementCountNextNew, ElementsNextNew} | Elements].

store_element_1_tuple(I, LeftI, RightI, Elements, K1, V1, N)
    when I =:= LeftI; I =:= RightI ->
    case erlang:element(I, Elements) of
        {K1, _} ->
            erlang:setelement(I, Elements, {K1, V1});
        {K2, _} = E when K2 > K1 ->
            erlang:setelement(I, Elements, {K1, K2, 2, [{K1, V1}, E]});
        {K2, _} = E -> % K2 < K1
            erlang:setelement(I, Elements, {K2, K1, 2, [E, {K1, V1}]});
        {KT1, KT2, N, ElementsNext} when is_list(ElementsNext) ->
            erlang:setelement(I, Elements, 
                {erlang:min(K1, KT1), erlang:max(K1, KT2), N,
                 store_element_1_tuple(erlang:round(N / 2), 1, N,
                                       erlang:list_to_tuple(ElementsNext),
                                       K1, V1, N)});
        {KT1, KT2, N, ElementsNext} when is_tuple(ElementsNext) ->
            erlang:setelement(I, Elements, 
                {erlang:min(K1, KT1), erlang:max(K1, KT2), N,
                 store_element_1_tuple(erlang:round(N / 2), 1, N,
                                       ElementsNext, K1, V1, N)});
        {KT1, KT2, ElementCountNext, ElementsNext} ->
            ElementsNextNew = store_element_1_list(ElementsNext, K1, V1, N),
            ElementCountNextNew = if
                ElementCountNext =:= N - 1 ->
                    erlang:length(ElementsNextNew);
                true ->
                    ElementCountNext + 1
            end,
            erlang:setelement(I, Elements,
                {erlang:min(K1, KT1), erlang:max(K1, KT2),
                 ElementCountNextNew, ElementsNextNew})
    end;
store_element_1_tuple(I, LeftI, RightI, Elements, K1, V1, N) ->
    case erlang:element(I, Elements) of
        {K1, _} ->
            erlang:setelement(I, Elements, {K1, V1});
        {K2, _} when K2 > K1 ->
            if
                LeftI =:= I - 1 ->
                    store_element_1_tuple(LeftI, LeftI, I,
                                          Elements, K1, V1, N);
                true ->
                    store_element_1_tuple(erlang:round((LeftI + I) / 2),
                                          LeftI, I,
                                          Elements, K1, V1, N)
            end;
        {_, _} -> % K2 < K1
            if
                RightI =:= I + 1 ->
                    store_element_1_tuple(RightI, I, RightI,
                                          Elements, K1, V1, N);
                true ->
                    store_element_1_tuple(erlang:round((I + RightI) / 2),
                                          I, RightI,
                                          Elements, K1, V1, N)
            end;
        {KT1, _, _, _} when KT1 > K1 ->
            if
                LeftI =:= I - 1 ->
                    store_element_1_tuple(LeftI, LeftI, I,
                                          Elements, K1, V1, N);
                true ->
                    store_element_1_tuple(erlang:round((LeftI + I) / 2),
                                          LeftI, I,
                                          Elements, K1, V1, N)
            end;
        {_, KT2, _, _} when KT2 < K1 ->
            if
                RightI =:= I + 1 ->
                    store_element_1_tuple(RightI, I, RightI,
                                          Elements, K1, V1, N);
                true ->
                    store_element_1_tuple(erlang:round((I + RightI) / 2),
                                          I, RightI,
                                          Elements, K1, V1, N)
            end;
        {KT1, KT2, N, ElementsNext} when is_list(ElementsNext) ->
            erlang:setelement(I, Elements,
                {erlang:min(K1, KT1), erlang:max(K1, KT2), N,
                 store_element_1_tuple(erlang:round(N / 2), 1, N,
                                       erlang:list_to_tuple(ElementsNext),
                                       K1, V1, N)});
        {KT1, KT2, N, ElementsNext} when is_tuple(ElementsNext) ->
            erlang:setelement(I, Elements,
                {erlang:min(K1, KT1), erlang:max(K1, KT2), N,
                 store_element_1_tuple(erlang:round(N / 2), 1, N,
                                       ElementsNext, K1, V1, N)});
        {KT1, KT2, ElementCountNext, ElementsNext} ->
            ElementsNextNew = store_element_1_list(ElementsNext, K1, V1, N),
            ElementCountNextNew = if
                ElementCountNext =:= N - 1 ->
                    erlang:length(ElementsNextNew);
                true ->
                    ElementCountNext + 1
            end,
            erlang:setelement(I, Elements,
                {erlang:min(K1, KT1), erlang:max(K1, KT2),
                 ElementCountNextNew, ElementsNextNew})
    end.
            
%%-------------------------------------------------------------------------
%% @doc
%% ===Internal test.===
%% @end
%%-------------------------------------------------------------------------
test() ->
    {3,0,[]} = A0 = ntree:new(3),
    {3,1,[{50,50}]} = A1 = ntree:store(50, 50, A0),
    {3,2,[{50,50},{100,100}]} = A2 = ntree:store(100, 100, A1),
    {3,3,[{25,25},{50,50},{100,100}]} = A3 = ntree:store(25, 25, A2),
    A4 = ntree:store(26, 26, A3),
    A5 = ntree:store(27, 27, A4),
    A6 = ntree:store(24, 24, A5),
    A7 = ntree:store(200, 200, A6),
    A8 = ntree:store(300, 300, A7),
    {3,3,
     {{24,27,2,[{24,25,2,[{24,24},{25,25}]},{26,27,2,[{26,26},{27,27}]}]},
      {50,50},
      {100,300,2,[{100,100},{200,300,2,[{200,200},{300,300}]}]}}} = A8,
    error = ntree:find(23, A8),
    {ok, 24} = ntree:find(24, A8),
    {ok, 25} = ntree:find(25, A8),
    {ok, 26} = ntree:find(26, A8),
    error = ntree:find(28, A8),
    {ok, 50} = ntree:find(50, A8),
    error = ntree:find(400, A8),
    {ok, 300} = ntree:find(300, A8),
    ok.

