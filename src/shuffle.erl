%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(shuffle).

-export([shuffle/1]).

% Fisher-Yates shuffle
shuffle(Array) ->
    shuffle(array:size(Array) - 1, Array).

shuffle(0, Array) ->
    Array;

shuffle(I, Array) ->
    J = random:uniform(I + 1) - 1,
    Temp = array:get(I, Array),
    NewArray = array:set(J, Temp, array:set(I, array:get(J, Array), Array)),
    shuffle(I - 1, NewArray).

