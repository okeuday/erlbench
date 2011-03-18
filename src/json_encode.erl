%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(json_encode).

-export([test/1]).

-include("erlbench.hrl").

-define(FILE1, "data/sample1.json").
-define(FILE2, "data/sample1_pretty.json").
-define(FILE3, "data/sample2.json").
-define(FILE4, "data/sample2_pretty.json").
-define(FILE5, "data/sample3.json").
-define(FILE6, "data/sample3_pretty.json").
-define(FILE7, "data/sample4.json").
-define(FILE8, "data/sample4_pretty.json").

test(_) ->
    {ok, FileData1} = file:read_file(?FILE1),
    {ok, FileData2} = file:read_file(?FILE2),
    {ok, FileData3} = file:read_file(?FILE3),
    {ok, FileData4} = file:read_file(?FILE4),
    {ok, FileData5} = file:read_file(?FILE5),
    {ok, FileData6} = file:read_file(?FILE6),
    {ok, FileData7} = file:read_file(?FILE7),
    {ok, FileData8} = file:read_file(?FILE8),

    F1Result = mochijson2:decode(FileData1),
    F2Result = mochijson2:decode(FileData2),
    F3Result = mochijson2:decode(FileData3),
    F4Result = mochijson2:decode(FileData4),
    F5Result = mochijson2:decode(FileData5),
    F6Result = mochijson2:decode(FileData6),
    F7Result = mochijson2:decode(FileData7),
    F8Result = mochijson2:decode(FileData8),

    {F1ejson, _} = timer:tc(ejson, encode, [F1Result]),
    {F2ejson, _} = timer:tc(ejson, encode, [F2Result]),
    {F3ejson, _} = timer:tc(ejson, encode, [F3Result]),
    {F4ejson, _} = timer:tc(ejson, encode, [F4Result]),
    {F5ejson, _} = timer:tc(ejson, encode, [F5Result]),
    {F6ejson, _} = timer:tc(ejson, encode, [F6Result]),
    {F7ejson, _} = timer:tc(ejson, encode, [F7Result]),
    {F8ejson, _} = timer:tc(ejson, encode, [F8Result]),

    %{F1jsx, _} = timer:tc(jsx, term_to_json, [F1Result]),
    %{F2jsx, _} = timer:tc(jsx, term_to_json, [F2Result]),
    %{F3jsx, _} = timer:tc(jsx, term_to_json, [F3Result]),
    %{F4jsx, _} = timer:tc(jsx, term_to_json, [F4Result]),
    %{F5jsx, _} = timer:tc(jsx, term_to_json, [F5Result]),
    %{F6jsx, _} = timer:tc(jsx, term_to_json, [F6Result]),
    %{F7jsx, _} = timer:tc(jsx, term_to_json, [F7Result]),
    %{F8jsx, _} = timer:tc(jsx, term_to_json, [F8Result]),

    {F1mochijson2, _} = timer:tc(mochijson2, encode, [F1Result]),
    {F2mochijson2, _} = timer:tc(mochijson2, encode, [F2Result]),
    {F3mochijson2, _} = timer:tc(mochijson2, encode, [F3Result]),
    {F4mochijson2, _} = timer:tc(mochijson2, encode, [F4Result]),
    {F5mochijson2, _} = timer:tc(mochijson2, encode, [F5Result]),
    {F6mochijson2, _} = timer:tc(mochijson2, encode, [F6Result]),
    {F7mochijson2, _} = timer:tc(mochijson2, encode, [F7Result]),
    {F8mochijson2, _} = timer:tc(mochijson2, encode, [F8Result]),

    %{F1rfc4627, _} = timer:tc(rfc4627, encode, [F1Result]),
    %{F2rfc4627, _} = timer:tc(rfc4627, encode, [F2Result]),
    %{F3rfc4627, _} = timer:tc(rfc4627, encode, [F3Result]),
    %{F4rfc4627, _} = timer:tc(rfc4627, encode, [F4Result]),
    %{F5rfc4627, _} = timer:tc(rfc4627, encode, [F5Result]),
    %{F6rfc4627, _} = timer:tc(rfc4627, encode, [F6Result]),
    %{F7rfc4627, _} = timer:tc(rfc4627, encode, [F7Result]),
    %{F8rfc4627, _} = timer:tc(rfc4627, encode, [F8Result]),

    %% results
    [
        #result{name = "file1 encode ejson",         get =  F1ejson},
        #result{name = "file2 encode ejson",         get =  F2ejson},
        #result{name = "file3 encode ejson",         get =  F3ejson},
        #result{name = "file4 encode ejson",         get =  F4ejson},
        #result{name = "file5 encode ejson",         get =  F5ejson},
        #result{name = "file6 encode ejson",         get =  F6ejson},
        #result{name = "file7 encode ejson",         get =  F7ejson},
        #result{name = "file8 encode ejson",         get =  F8ejson},

        %#result{name = "file1 encode jsx",           get =  F1jsx},
        %#result{name = "file2 encode jsx",           get =  F2jsx},
        %#result{name = "file3 encode jsx",           get =  F3jsx},
        %#result{name = "file4 encode jsx",           get =  F4jsx},
        %#result{name = "file5 encode jsx",           get =  F5jsx},
        %#result{name = "file6 encode jsx",           get =  F6jsx},
        %#result{name = "file7 encode jsx",           get =  F7jsx},
        %#result{name = "file8 encode jsx",           get =  F8jsx},

        #result{name = "file1 encode mochijson2",    get =  F1mochijson2},
        #result{name = "file2 encode mochijson2",    get =  F2mochijson2},
        #result{name = "file3 encode mochijson2",    get =  F3mochijson2},
        #result{name = "file4 encode mochijson2",    get =  F4mochijson2},
        #result{name = "file5 encode mochijson2",    get =  F5mochijson2},
        #result{name = "file6 encode mochijson2",    get =  F6mochijson2},
        #result{name = "file7 encode mochijson2",    get =  F7mochijson2},
        #result{name = "file8 encode mochijson2",    get =  F8mochijson2}

        %#result{name = "file1 encode rfc4627",       get =  F1rfc4627},
        %#result{name = "file2 encode rfc4627",       get =  F2rfc4627},
        %#result{name = "file3 encode rfc4627",       get =  F3rfc4627},
        %#result{name = "file4 encode rfc4627",       get =  F4rfc4627},
        %#result{name = "file5 encode rfc4627",       get =  F5rfc4627},
        %#result{name = "file6 encode rfc4627",       get =  F6rfc4627},
        %#result{name = "file7 encode rfc4627",       get =  F7rfc4627},
        %#result{name = "file8 encode rfc4627",       get =  F8rfc4627}
    ].

