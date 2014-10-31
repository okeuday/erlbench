%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(json_decode).

-export([test/1, run/3]).

-include("erlbench.hrl").

-define(FILE1, "data/sample1.json").
-define(FILE2, "data/sample1_pretty.json").
-define(FILE3, "data/sample2.json").
-define(FILE4, "data/sample2_pretty.json").
-define(FILE5, "data/sample3.json").
-define(FILE6, "data/sample3_pretty.json").
-define(FILE7, "data/sample4.json").
-define(FILE8, "data/sample4_pretty.json").

run(1, F, A) ->
    F(A);
run(N, F, A) ->
    F(A),
    run(N - 1, F, A).

test(N) ->
    {ok, FileData1} = file:read_file(?FILE1),
    {ok, FileData2} = file:read_file(?FILE2),
    {ok, FileData3} = file:read_file(?FILE3),
    {ok, FileData4} = file:read_file(?FILE4),
    {ok, FileData5} = file:read_file(?FILE5),
    {ok, FileData6} = file:read_file(?FILE6),
    {ok, FileData7} = file:read_file(?FILE7),
    {ok, FileData8} = file:read_file(?FILE8),

    {F1ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData1]),
    {F2ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData2]),
    {F3ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData3]),
    {F4ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData4]),
    {F5ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData5]),
    {F6ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData6]),
    {F7ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData7]),
    {F8ejson, _} = timer:tc(json_decode, run, [N, fun ejson:decode/1,
                                               FileData8]),

    {F1jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData1]),
    {F2jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData2]),
    {F3jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData3]),
    {F4jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData4]),
    {F5jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData5]),
    {F6jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData6]),
    {F7jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData7]),
    {F8jsx, _} = timer:tc(json_decode, run, [N, fun jsx:decode/1,
                                             FileData8]),

    {F1mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData1]),
    {F2mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData2]),
    {F3mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData3]),
    {F4mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData4]),
    {F5mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData5]),
    {F6mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData6]),
    {F7mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData7]),
    {F8mochijson2, _} = timer:tc(json_decode, run, [N, fun mochijson2:decode/1,
                                                    FileData8]),

    {F1rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData1]),
    {F2rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData2]),
    {F3rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData3]),
    {F4rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData4]),
    {F5rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData5]),
    {F6rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData6]),
    {F7rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData7]),
    {F8rfc4627, _} = timer:tc(json_decode, run, [N, fun rfc4627:decode/1,
                                                 FileData8]),

    %% results
    [
        #result{name = "file1 decode ejson",         get =  F1ejson},
        #result{name = "file2 decode ejson",         get =  F2ejson},
        #result{name = "file3 decode ejson",         get =  F3ejson},
        #result{name = "file4 decode ejson",         get =  F4ejson},
        #result{name = "file5 decode ejson",         get =  F5ejson},
        #result{name = "file6 decode ejson",         get =  F6ejson},
        #result{name = "file7 decode ejson",         get =  F7ejson},
        #result{name = "file8 decode ejson",         get =  F8ejson},

        #result{name = "file1 decode jsx",           get =  F1jsx},
        #result{name = "file2 decode jsx",           get =  F2jsx},
        #result{name = "file3 decode jsx",           get =  F3jsx},
        #result{name = "file4 decode jsx",           get =  F4jsx},
        #result{name = "file5 decode jsx",           get =  F5jsx},
        #result{name = "file6 decode jsx",           get =  F6jsx},
        #result{name = "file7 decode jsx",           get =  F7jsx},
        #result{name = "file8 decode jsx",           get =  F8jsx},

        #result{name = "file1 decode mochijson2",    get =  F1mochijson2},
        #result{name = "file2 decode mochijson2",    get =  F2mochijson2},
        #result{name = "file3 decode mochijson2",    get =  F3mochijson2},
        #result{name = "file4 decode mochijson2",    get =  F4mochijson2},
        #result{name = "file5 decode mochijson2",    get =  F5mochijson2},
        #result{name = "file6 decode mochijson2",    get =  F6mochijson2},
        #result{name = "file7 decode mochijson2",    get =  F7mochijson2},
        #result{name = "file8 decode mochijson2",    get =  F8mochijson2},

        #result{name = "file1 decode rfc4627",       get =  F1rfc4627},
        #result{name = "file2 decode rfc4627",       get =  F2rfc4627},
        #result{name = "file3 decode rfc4627",       get =  F3rfc4627},
        #result{name = "file4 decode rfc4627",       get =  F4rfc4627},
        #result{name = "file5 decode rfc4627",       get =  F5rfc4627},
        #result{name = "file6 decode rfc4627",       get =  F6rfc4627},
        #result{name = "file7 decode rfc4627",       get =  F7rfc4627},
        #result{name = "file8 decode rfc4627",       get =  F8rfc4627}
    ].

