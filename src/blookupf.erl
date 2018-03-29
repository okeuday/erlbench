%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Binary Lookup With Fixed Sized Pairs.==
%%% Keep all binary key/value data in a single continuous binary to
%%% minimize Erlang memory usage.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2015-2017 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(blookupf).
-author('mjtruog at protonmail dot com').

%% external interface
-export([find/2,
         new/1,
         new/2,
         store/3,
         test/0]).

-ifndef(KEY_SIZE_MAX_BYTES).
-define(KEY_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(VALUE_SIZE_MAX_BYTES).
-define(VALUE_SIZE_MAX_BYTES, 4).
-endif.
-define(KEY_SIZE_TYPE,
        :?KEY_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).
-define(VALUE_SIZE_TYPE,
        :?VALUE_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).

-record(blookupf,
    {
        size = 0 :: non_neg_integer(),
        data = <<>> :: binary(),
        free_size :: pos_integer() | undefined,
        block_size :: pos_integer()
    }).

-type state() :: #blookupf{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in the binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec find(Key :: binary(),
           Lookup :: #blookupf{}) ->
    {ok, Value :: binary()} |
    error.

find(<<>>, _) ->
    error;
find(Key,
     #blookupf{size = Size,
               data = Data,
               block_size = BlockSize})
    when is_binary(Key) ->
    case lookup(Key, Size, Data, BlockSize) of
        {true, _, Value} ->
            {ok, Value};
        {false, _} ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new(BlockSizeBytes :: pos_integer()) ->
    #blookupf{}.

new(BlockSizeBytes)
    when is_integer(BlockSizeBytes), BlockSizeBytes > 0 ->
    #blookupf{free_size = undefined,
              block_size = BlockSizeBytes}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new binary lookup with a total size limit.===
%% @end
%%-------------------------------------------------------------------------

-spec new(BlockSizeBytes :: pos_integer(),
          TotalSizeMaxBytes :: pos_integer()) ->
    #blookupf{}.

new(BlockSizeBytes, TotalSizeMaxBytes)
    when is_integer(BlockSizeBytes), BlockSizeBytes > 0,
         is_integer(TotalSizeMaxBytes), TotalSizeMaxBytes > 0,
         TotalSizeMaxBytes > BlockSizeBytes ->
    #blookupf{free_size = TotalSizeMaxBytes,
              block_size = BlockSizeBytes}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key/value pair in the binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: binary(),
            Value :: binary(),
            Lookup :: #blookupf{}) ->
    #blookupf{}.

store(<<>>, _, _) ->
    erlang:exit(badarg);
store(Key, Value,
      #blookupf{size = SizeOld,
                data = DataOld,
                free_size = FreeSizeOld,
                block_size = BlockSize} = Lookup)
    when is_binary(Key), is_binary(Value) ->
    {SizeNew, DataNew,
     FreeSizeNew} = case lookup(Key, SizeOld, DataOld, BlockSize) of
        {true, I, _} ->
            store_key_value_replace(Key, Value, I, SizeOld,
                                    DataOld, FreeSizeOld, BlockSize);
        {false, I} ->
            store_key_value_insert(Key, Value, I, SizeOld,
                                   DataOld, FreeSizeOld, BlockSize)
    end,
    Lookup#blookupf{size = SizeNew,
                    data = DataNew,
                    free_size = FreeSizeNew}.

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    % key_size_max_bytes + value_size_max_bytes == 22 (for tests below)
    Lookup0 = blookupf:new(?KEY_SIZE_MAX_BYTES + ?VALUE_SIZE_MAX_BYTES + 22,
                           1048576),
    error = blookupf:find(<<>>, Lookup0),
    {'EXIT', badarg} = (catch blookupf:store(<<>>, <<"ignored">>, Lookup0)),
    Lookup1 = blookupf:store(<<"abcd">>, <<"value0">>, Lookup0),
    {ok, <<"value0">>} = blookupf:find(<<"abcd">>, Lookup1),
    error = blookupf:find(<<"abc">>, Lookup1),
    Lookup2 = blookupf:store(<<"abcc">>, <<"value1">>, Lookup1),
    Lookup3 = blookupf:store(<<"abcb">>, <<"value2">>, Lookup2),
    Lookup4 = blookupf:store(<<"bc">>, <<"value__3">>, Lookup3),
    Lookup5 = blookupf:store(<<"aaaaaaaaaaaa">>, <<"value00004">>, Lookup4),
    Lookup6 = blookupf:store(<<"a">>, <<"value5">>, Lookup5),
    {ok, <<"value0">>} = blookupf:find(<<"abcd">>, Lookup6),
    {ok, <<"value2">>} = blookupf:find(<<"abcb">>, Lookup6),
    {ok, <<"value__3">>} = blookupf:find(<<"bc">>, Lookup6),
    error = blookupf:find(<<"aaaaaaaaaaa">>, Lookup6),
    {ok, <<"value00004">>} = blookupf:find(<<"aaaaaaaaaaaa">>, Lookup6),
    {ok, <<"value5">>} = blookupf:find(<<"a">>, Lookup6),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

lookup(_, 0, _, _) ->
    {false, 0};
lookup(Key, Size, Data, BlockSize) ->
    I = Size div 2,
    {KeyCompare, Value} = lookup_key_value(I, Data, BlockSize),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            IMax = Size - 1,
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Data, BlockSize)
            end;
        Key < KeyCompare ->
            IMin = 0,
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Data, BlockSize)
            end
    end.

lookup_loop(I, I, Key, Data, BlockSize) ->
    {KeyCompare, Value} = lookup_key_value(I, Data, BlockSize),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            {false, I + 1};
        Key < KeyCompare ->
            {false, I}
    end;
lookup_loop(IMin, IMax, Key, Data, BlockSize) ->
    I = (IMin + IMax) div 2,
    {KeyCompare, Value} = lookup_key_value(I, Data, BlockSize),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Data, BlockSize)
            end;
        Key < KeyCompare ->
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Data, BlockSize)
            end
    end.

lookup_key_value(0,
                 <<KeySize?KEY_SIZE_TYPE,
                   ValueSize?VALUE_SIZE_TYPE,
                   Pair/binary>>, _) ->
    <<Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8,
      _/binary>> = Pair,
    {Key, Value};
lookup_key_value(I, Data, BlockSize) ->
    Index = I * BlockSize,
    PairSize = BlockSize - (?KEY_SIZE_MAX_BYTES + ?VALUE_SIZE_MAX_BYTES),
    <<_:Index/binary-unit:8,
      KeySize?KEY_SIZE_TYPE,
      ValueSize?VALUE_SIZE_TYPE,
      Pair:PairSize/binary-unit:8,
      _/binary>> = Data,
    <<Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8,
      _/binary>> = Pair,
    {Key, Value}.

store_key_value_replace(Key, Value, I, Size, Data, FreeSize, BlockSize) ->
    Index = I * BlockSize,
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    BlockBufferBitsNew = block_size_check(KeySizeNew + ValueSizeNew,
                                          BlockSize),
    <<DataPart0:Index/binary-unit:8,
      _:BlockSize/binary-unit:8,
      DataPart1/binary>> = Data,
    {Size,
     <<DataPart0:Index/binary-unit:8,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew,
       DataPart1/binary>>,
     FreeSize}.

store_key_value_insert(Key, Value, 0, 0, <<>>, FreeSize, BlockSize) ->
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    BlockBufferBitsNew = block_size_check(KeySizeNew + ValueSizeNew,
                                          BlockSize),
    FreeSizeNew = if
        FreeSize =:= undefined ->
            undefined;
        FreeSize >= BlockSize ->
            FreeSize - BlockSize;
        true ->
            erlang:exit(badarg)
    end,
    {1,
     <<KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew>>,
     FreeSizeNew};
store_key_value_insert(Key, Value, I, Size, Data, FreeSize, BlockSize) ->
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    BlockBufferBitsNew = block_size_check(KeySizeNew + ValueSizeNew,
                                          BlockSize),
    Index = I * BlockSize,
    <<DataPart0:Index/binary-unit:8,
      DataPart1/binary>> = Data,
    FreeSizeNew = if
        FreeSize =:= undefined ->
            undefined;
        FreeSize >= BlockSize ->
            FreeSize - BlockSize;
        true ->
            erlang:exit(badarg)
    end,
    {Size + 1,
     <<DataPart0:Index/binary-unit:8,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew,
       DataPart1/binary>>,
     FreeSizeNew}.

block_size_check(BytesStored, BlockSize)
    when is_integer(BytesStored) ->
    BytesStoredTotal = ?KEY_SIZE_MAX_BYTES + ?VALUE_SIZE_MAX_BYTES +
                       BytesStored,
    if
        BytesStoredTotal > BlockSize ->
            erlang:exit(badarg);
        true ->
            (BlockSize - BytesStoredTotal) * 8
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

-endif.

