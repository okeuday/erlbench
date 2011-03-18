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

    F1ejsonResult = ejson:decode(FileData1),
    F2ejsonResult = ejson:decode(FileData2),
    F3ejsonResult = ejson:decode(FileData3),
    F4ejsonResult = ejson:decode(FileData4),
    F5ejsonResult = ejson:decode(FileData5),
    F6ejsonResult = ejson:decode(FileData6),
    F7ejsonResult = ejson:decode(FileData7),
    F8ejsonResult = ejson:decode(FileData8),

    F1jsxResult = jsx:json_to_term(FileData1),
    F2jsxResult = jsx:json_to_term(FileData2),
    F3jsxResult = jsx:json_to_term(FileData3),
    F4jsxResult = jsx:json_to_term(FileData4),
    F5jsxResult = jsx:json_to_term(FileData5),
    F6jsxResult = jsx:json_to_term(FileData6),
    F7jsxResult = jsx:json_to_term(FileData7),
    F8jsxResult = jsx:json_to_term(FileData8),

    F1mochijson2Result = mochijson2:decode(FileData1),
    F2mochijson2Result = mochijson2:decode(FileData2),
    F3mochijson2Result = mochijson2:decode(FileData3),
    F4mochijson2Result = mochijson2:decode(FileData4),
    F5mochijson2Result = mochijson2:decode(FileData5),
    F6mochijson2Result = mochijson2:decode(FileData6),
    F7mochijson2Result = mochijson2:decode(FileData7),
    F8mochijson2Result = mochijson2:decode(FileData8),

    {ok, F1rfc4627Result, _} = rfc4627:decode(FileData1),
    {ok, F2rfc4627Result, _} = rfc4627:decode(FileData2),
    {ok, F3rfc4627Result, _} = rfc4627:decode(FileData3),
    {ok, F4rfc4627Result, _} = rfc4627:decode(FileData4),
    {ok, F5rfc4627Result, _} = rfc4627:decode(FileData5),
    {ok, F6rfc4627Result, _} = rfc4627:decode(FileData6),
    {ok, F7rfc4627Result, _} = rfc4627:decode(FileData7),
    {ok, F8rfc4627Result, _} = rfc4627:decode(FileData8),

    {F1ejson, _} = timer:tc(ejson, encode, [F1ejsonResult]),
    {F2ejson, _} = timer:tc(ejson, encode, [F2ejsonResult]),
    {F3ejson, _} = timer:tc(ejson, encode, [F3ejsonResult]),
    {F4ejson, _} = timer:tc(ejson, encode, [F4ejsonResult]),
    {F5ejson, _} = timer:tc(ejson, encode, [F5ejsonResult]),
    {F6ejson, _} = timer:tc(ejson, encode, [F6ejsonResult]),
    {F7ejson, _} = timer:tc(ejson, encode, [F7ejsonResult]),
    {F8ejson, _} = timer:tc(ejson, encode, [F8ejsonResult]),

    {F1jsx, _} = timer:tc(jsx, term_to_json, [F1jsxResult]),
    {F2jsx, _} = timer:tc(jsx, term_to_json, [F2jsxResult]),
    {F3jsx, _} = timer:tc(jsx, term_to_json, [F3jsxResult]),
    {F4jsx, _} = timer:tc(jsx, term_to_json, [F4jsxResult]),
    {F5jsx, _} = timer:tc(jsx, term_to_json, [F5jsxResult]),
    {F6jsx, _} = timer:tc(jsx, term_to_json, [F6jsxResult]),
    {F7jsx, _} = timer:tc(jsx, term_to_json, [F7jsxResult]),
    {F8jsx, _} = timer:tc(jsx, term_to_json, [F8jsxResult]),

    {F1mochijson2, _} = timer:tc(mochijson2, encode, [F1mochijson2Result]),
    {F2mochijson2, _} = timer:tc(mochijson2, encode, [F2mochijson2Result]),
    {F3mochijson2, _} = timer:tc(mochijson2, encode, [F3mochijson2Result]),
    {F4mochijson2, _} = timer:tc(mochijson2, encode, [F4mochijson2Result]),
    {F5mochijson2, _} = timer:tc(mochijson2, encode, [F5mochijson2Result]),
    {F6mochijson2, _} = timer:tc(mochijson2, encode, [F6mochijson2Result]),
    {F7mochijson2, _} = timer:tc(mochijson2, encode, [F7mochijson2Result]),
    {F8mochijson2, _} = timer:tc(mochijson2, encode, [F8mochijson2Result]),

    {F1rfc4627, _} = timer:tc(rfc4627, encode, [F1rfc4627Result]),
    {F2rfc4627, _} = timer:tc(rfc4627, encode, [F2rfc4627Result]),
    {F3rfc4627, _} = timer:tc(rfc4627, encode, [F3rfc4627Result]),
    {F4rfc4627, _} = timer:tc(rfc4627, encode, [F4rfc4627Result]),
    {F5rfc4627, _} = timer:tc(rfc4627, encode, [F5rfc4627Result]),
    {F6rfc4627, _} = timer:tc(rfc4627, encode, [F6rfc4627Result]),
    {F7rfc4627, _} = timer:tc(rfc4627, encode, [F7rfc4627Result]),
    {F8rfc4627, _} = timer:tc(rfc4627, encode, [F8rfc4627Result]),

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

        #result{name = "file1 encode jsx",           get =  F1jsx},
        #result{name = "file2 encode jsx",           get =  F2jsx},
        #result{name = "file3 encode jsx",           get =  F3jsx},
        #result{name = "file4 encode jsx",           get =  F4jsx},
        #result{name = "file5 encode jsx",           get =  F5jsx},
        #result{name = "file6 encode jsx",           get =  F6jsx},
        #result{name = "file7 encode jsx",           get =  F7jsx},
        #result{name = "file8 encode jsx",           get =  F8jsx},

        #result{name = "file1 encode mochijson2",    get =  F1mochijson2},
        #result{name = "file2 encode mochijson2",    get =  F2mochijson2},
        #result{name = "file3 encode mochijson2",    get =  F3mochijson2},
        #result{name = "file4 encode mochijson2",    get =  F4mochijson2},
        #result{name = "file5 encode mochijson2",    get =  F5mochijson2},
        #result{name = "file6 encode mochijson2",    get =  F6mochijson2},
        #result{name = "file7 encode mochijson2",    get =  F7mochijson2},
        #result{name = "file8 encode mochijson2",    get =  F8mochijson2},

        #result{name = "file1 encode rfc4627",       get =  F1rfc4627},
        #result{name = "file2 encode rfc4627",       get =  F2rfc4627},
        #result{name = "file3 encode rfc4627",       get =  F3rfc4627},
        #result{name = "file4 encode rfc4627",       get =  F4rfc4627},
        #result{name = "file5 encode rfc4627",       get =  F5rfc4627},
        #result{name = "file6 encode rfc4627",       get =  F6rfc4627},
        #result{name = "file7 encode rfc4627",       get =  F7rfc4627},
        #result{name = "file8 encode rfc4627",       get =  F8rfc4627}
    ].

