%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(run).

-export([test/0]).

-include("erlbench.hrl").

-define(RUNS, 10).

% parameters for the runs

%-define(COUNTS, [100000]).
%-define(TESTS, [uuid_creation]).

%-define(COUNTS, [100]).
%-define(TESTS, [json_encode, json_decode]).

-define(COUNTS, [10000]).
-define(TESTS, [pseudo_randomness]).

%-define(COUNTS, [1000000]).
%-define(TESTS, [pqueue_priority0, pqueue_priorities2, pqueue_priorities41,
%                pqueue_priorities64]).

%-define(COUNTS, [1000, 10000, 50000]).
%-define(TESTS, [string_key, integer_key, list_traversal]).
%-define(COUNTS, [50, 100, 250, 500, 1000, 2000, 4000,
%                 8000, 16000, 32000, 64000]).
%-define(TESTS, [integer_key]).
%-define(TESTS, [string_key, integer_key]).

test() ->
    <<Random1:16/unsigned-integer,
      Random2:16/unsigned-integer,
      Random3:16/unsigned-integer>> = crypto:rand_bytes(6),
    random:seed(Random1, Random2, Random3),
    lists:foreach(fun(Test) ->
        io:format("TEST ~p~n", [Test]),
        lists:foreach(fun(Count) ->
            [Result1 | RemainingResults] = lists:map(fun(_) ->
                Results = Test:test(Count),
                trie:new(Results)
            end, lists:seq(1, ?RUNS)),
            ResultSums = lists:foldl(fun(R, Sums) ->
                trie:merge(fun(_, Value1, Value2) ->
                    Value1#result{
                        get = sum(Value1#result.get, Value2#result.get),
                        set = sum(Value1#result.set, Value2#result.set),
                        update = sum(Value1#result.update, Value2#result.update)
                    }
                end, Sums, R)
            end, Result1, RemainingResults),
            FinalResults = trie:map(fun(_, Value) ->
                Value#result{
                    get = divide(Value#result.get, ?RUNS),
                    set = divide(Value#result.set, ?RUNS),
                    update = divide(Value#result.update, ?RUNS)
                }
            end, ResultSums),
            Min = trie:foldl(fun(_, Value, M) ->
                M#result{
                    get = minimum(Value#result.get, M#result.get),
                    set = minimum(Value#result.set, M#result.set),
                    update = minimum(Value#result.update, M#result.update)
                }
            end, #result{}, FinalResults),

            Header = io_lib:format("N == ~w (~w runs)~n", [Count, ?RUNS]),
            Output = trie:foldl(fun(_, Value, L) ->
                if
                    Value#result.get /= undefined,
                    Value#result.set /= undefined,
                    Value#result.update /= undefined ->
                        MinGet = round(Value#result.get /
                                       Min#result.get, 1),
                        MinSet = round(Value#result.set /
                                       Min#result.set, 1),
                        MinUpdate = round(Value#result.update /
                                          Min#result.update, 1),
                        L ++ io_lib:format("~20s "
                                           "get: ~10w us (~5w), "
                                           "set: ~10w us (~5w), "
                                           "update: ~10w us (~5w)~n", [
                                            Value#result.name,
                                            Value#result.get,
                                            MinGet,
                                            Value#result.set,
                                            MinSet,
                                            Value#result.update,
                                            MinUpdate]);
                    Value#result.get /= undefined,
                    Value#result.set /= undefined ->
                        MinGet = round(Value#result.get /
                                       Min#result.get, 1),
                        MinSet = round(Value#result.set /
                                       Min#result.set, 1),
                        L ++ io_lib:format("~20s "
                                           "get: ~10w us (~5w), "
                                           "set: ~10w us (~5w)~n", [
                                            Value#result.name,
                                            Value#result.get,
                                            MinGet,
                                            Value#result.set,
                                            MinSet]);
                    Value#result.get /= undefined ->
                        MinGet = round(Value#result.get /
                                       Min#result.get, 1),
                        L ++ io_lib:format("~20s "
                                           "get: ~10w us (~5w)~n", [
                                            Value#result.name,
                                            Value#result.get,
                                            MinGet])
                end
            end, Header, FinalResults),
            io:format("~s", [Output])
        end, ?COUNTS)
    end, ?TESTS).

sum(undefined, undefined) ->
    undefined;

sum(I1, I2)
    when is_integer(I1), is_integer(I2) ->
    I1 + I2.

divide(undefined, _) ->
    undefined;

divide(I, N)
    when is_integer(I), is_integer(N) ->
    round(I / N, 1).

minimum(undefined, undefined) ->
    undefined;

minimum(undefined, I)
    when is_number(I) ->
    I;

minimum(I, undefined)
    when is_number(I) ->
    I;
    
minimum(I1, I2)
    when is_number(I1), is_number(I2) ->
    erlang:min(I1, I2).

round(I, N)
    when is_integer(N) ->
    J = math:pow(10.0, N),
    erlang:round(I * J) / J.

