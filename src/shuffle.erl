%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

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

