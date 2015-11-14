%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Binary Lookup With Variably Sized Pairs.==
%%% Keep all binary key/value data in a single continuous binary to
%%% minimize Erlang memory usage.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(blookupv).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([find/2,
         new/0,
         new/1,
         store/3,
         test/0]).

-ifndef(KEY_SIZE_MAX_BYTES).
-define(KEY_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(VALUE_SIZE_MAX_BYTES).
-define(VALUE_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(INDEX_SIZE_MAX_BYTES).
-define(INDEX_SIZE_MAX_BYTES, 4).
-endif.
-ifndef(BLOCK_SIZE_BYTES).
-define(BLOCK_SIZE_BYTES, 1). % 1 to disable blocks
-endif.
-define(INDEXES_ELEMENT_SIZE_BYTES,
        (?INDEX_SIZE_MAX_BYTES + ?KEY_SIZE_MAX_BYTES + ?VALUE_SIZE_MAX_BYTES)).
-define(INDEX_SIZE_TYPE,
        :?INDEX_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).
-define(KEY_SIZE_TYPE,
        :?KEY_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).
-define(VALUE_SIZE_TYPE,
        :?VALUE_SIZE_MAX_BYTES/big-unsigned-integer-unit:8).

-record(blookupv,
    {
        size = 0 :: non_neg_integer(),
        indexes = <<>> :: binary(),
        data = <<>> :: binary(),
        free_size :: pos_integer() | undefined
    }).

-type state() :: #blookupv{}.
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
           Lookup :: #blookupv{}) ->
    {ok, Value :: binary()} |
    error.

find(<<>>, _) ->
    error;
find(Key,
     #blookupv{size = Size,
               indexes = Indexes,
               data = Data})
    when is_binary(Key) ->
    case lookup(Key, Size, Indexes, Data) of
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

-spec new() ->
    #blookupv{}.

new() ->
    #blookupv{free_size = undefined}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new binary lookup with a total size limit.===
%% @end
%%-------------------------------------------------------------------------

-spec new(TotalSizeMaxBytes :: pos_integer()) ->
    #blookupv{}.

new(TotalSizeMaxBytes)
    when is_integer(TotalSizeMaxBytes), TotalSizeMaxBytes > 0 ->
    #blookupv{free_size = TotalSizeMaxBytes}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key/value pair in the binary lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: binary(),
            Value :: binary(),
            Lookup :: #blookupv{}) ->
    #blookupv{}.

store(<<>>, _, _) ->
    erlang:exit(badarg);
store(Key, Value,
      #blookupv{size = SizeOld,
                indexes = IndexesOld,
                data = DataOld,
                free_size = FreeSizeOld} = Lookup)
    when is_binary(Key), is_binary(Value) ->
    {SizeNew, IndexesNew, DataNew,
     FreeSizeNew} = case lookup(Key, SizeOld, IndexesOld, DataOld) of
        {true, I, _} ->
            store_key_value_replace(Key, Value, I, SizeOld,
                                    IndexesOld, DataOld, FreeSizeOld);
        {false, I} ->
            store_key_value_insert(Key, Value, I, SizeOld,
                                   IndexesOld, DataOld, FreeSizeOld)
    end,
    Lookup#blookupv{size = SizeNew,
                    indexes = IndexesNew,
                    data = DataNew,
                    free_size = FreeSizeNew}.

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    Lookup0 = blookupv:new(1048576),
    error = blookupv:find(<<>>, Lookup0),
    {'EXIT', badarg} = (catch blookupv:store(<<>>, <<"ignored">>, Lookup0)),
    Lookup1 = blookupv:store(<<"abcd">>, <<"value0">>, Lookup0),
    {ok, <<"value0">>} = blookupv:find(<<"abcd">>, Lookup1),
    error = blookupv:find(<<"abc">>, Lookup1),
    Lookup2 = blookupv:store(<<"abcc">>, <<"value1">>, Lookup1),
    Lookup3 = blookupv:store(<<"abcb">>, <<"value2">>, Lookup2),
    Lookup4 = blookupv:store(<<"bc">>, <<"value__3">>, Lookup3),
    Lookup5 = blookupv:store(<<"aaaaaaaaaaaa">>, <<"value00004">>, Lookup4),
    Lookup6 = blookupv:store(<<"a">>, <<"value5">>, Lookup5),
    {ok, <<"value0">>} = blookupv:find(<<"abcd">>, Lookup6),
    {ok, <<"value2">>} = blookupv:find(<<"abcb">>, Lookup6),
    {ok, <<"value__3">>} = blookupv:find(<<"bc">>, Lookup6),
    error = blookupv:find(<<"aaaaaaaaaaa">>, Lookup6),
    {ok, <<"value00004">>} = blookupv:find(<<"aaaaaaaaaaaa">>, Lookup6),
    {ok, <<"value5">>} = blookupv:find(<<"a">>, Lookup6),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

lookup(_, 0, _, _) ->
    {false, 0};
lookup(Key, Size, Indexes, Data) ->
    I = Size div 2,
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            IMax = Size - 1,
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Indexes, Data)
            end;
        Key < KeyCompare ->
            IMin = 0,
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Indexes, Data)
            end
    end.

lookup_loop(I, I, Key, Indexes, Data) ->
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            {false, I + 1};
        Key < KeyCompare ->
            {false, I}
    end;
lookup_loop(IMin, IMax, Key, Indexes, Data) ->
    I = (IMin + IMax) div 2,
    {KeyCompare, Value} = lookup_key_value(I, Indexes, Data),
    if
        Key == KeyCompare ->
            {true, I, Value};
        Key > KeyCompare ->
            if
                I == IMax ->
                    {false, I + 1};
                true ->
                    lookup_loop(I + 1, IMax, Key, Indexes, Data)
            end;
        Key < KeyCompare ->
            if
                I == IMin ->
                    {false, I};
                true ->
                    lookup_loop(IMin, I - 1, Key, Indexes, Data)
            end
    end.

lookup_key_value(0,
                 <<0?INDEX_SIZE_TYPE,
                   KeySize?KEY_SIZE_TYPE,
                   ValueSize?VALUE_SIZE_TYPE>>,
                 Data) ->
    <<Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8,
      _/binary>> = Data,
    {Key, Value};
lookup_key_value(I, Indexes, Data) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    <<_:Offset/binary-unit:8,
      Index?INDEX_SIZE_TYPE,
      KeySize?KEY_SIZE_TYPE,
      ValueSize?VALUE_SIZE_TYPE,
      _/binary>> = Indexes,
    <<_:Index/binary-unit:8,
      Key:KeySize/binary-unit:8,
      Value:ValueSize/binary-unit:8,
      _/binary>> = Data,
    {Key, Value}.

store_key_value_replace(Key, Value, I, Size, Indexes, Data, FreeSize) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    <<IndexesPart0:Offset/binary-unit:8,
      Index?INDEX_SIZE_TYPE,
      KeySizeOld?KEY_SIZE_TYPE,
      ValueSizeOld?VALUE_SIZE_TYPE,
      IndexesPart1Old/binary>> = Indexes,
    {BlockSizeOld, BlockBufferBitsOld} = block_size(KeySizeOld + ValueSizeOld),
    {BlockSizeNew, BlockBufferBitsNew} = block_size(KeySizeNew + ValueSizeNew),
    Difference = BlockSizeNew - BlockSizeOld,
    FreeSizeNew = if
        FreeSize =:= undefined ->
            undefined;
        FreeSize >= Difference ->
            FreeSize - Difference;
        true ->
            erlang:exit(badarg)
    end,
    IndexesPart1New = indexes_adjust(IndexesPart1Old, Difference),
    <<DataPart0:Index/binary-unit:8,
      _:KeySizeOld/binary-unit:8,
      _:ValueSizeOld/binary-unit:8,
      0:BlockBufferBitsOld,
      DataPart1/binary>> = Data,
    {Size,
     <<IndexesPart0:Offset/binary-unit:8,
       Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       IndexesPart1New/binary>>,
     <<DataPart0:Index/binary-unit:8,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew,
       DataPart1/binary>>,
     FreeSizeNew}.

store_key_value_insert(Key, Value, 0, 0, <<>>, <<>>, FreeSize) ->
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    Index = 0,
    {BlockSizeNew, BlockBufferBitsNew} = block_size(KeySizeNew + ValueSizeNew),
    FreeSizeNew = if
        FreeSize =:= undefined ->
            undefined;
        FreeSize >= BlockSizeNew ->
            FreeSize - BlockSizeNew;
        true ->
            erlang:exit(badarg)
    end,
    {1,
     <<Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE>>,
     <<Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew>>,
     FreeSizeNew};
store_key_value_insert(Key, Value, I, Size, Indexes, Data, FreeSize) ->
    Offset = I * ?INDEXES_ELEMENT_SIZE_BYTES,
    KeySizeNew = erlang:byte_size(Key),
    ValueSizeNew = erlang:byte_size(Value),
    <<IndexesPart0:Offset/binary-unit:8,
      IndexesPart1Old/binary>> = Indexes,
    {BlockSizeNew, BlockBufferBitsNew} = block_size(KeySizeNew + ValueSizeNew),
    FreeSizeNew = if
        FreeSize =:= undefined ->
            undefined;
        FreeSize >= BlockSizeNew ->
            FreeSize - BlockSizeNew;
        true ->
            erlang:exit(badarg)
    end,
    Index = case IndexesPart1Old of
        <<>> ->
            OffsetPrevious = (I - 1) * ?INDEXES_ELEMENT_SIZE_BYTES,
            <<_:OffsetPrevious/binary-unit:8,
              IndexPrevious?INDEX_SIZE_TYPE,
              KeySizePrevious?KEY_SIZE_TYPE,
              ValueSizePrevious?VALUE_SIZE_TYPE>> = IndexesPart0,
            {BlockSizePrevious, _} = block_size(KeySizePrevious +
                                                ValueSizePrevious),
            IndexPrevious + BlockSizePrevious;
        <<IndexValue?INDEX_SIZE_TYPE,
          _/binary>> ->
            IndexValue
    end,
    IndexesPart1New = indexes_adjust(IndexesPart1Old,
                                     BlockSizeNew),
    <<DataPart0:Index/binary-unit:8,
      DataPart1/binary>> = Data,
    {Size + 1,
     <<IndexesPart0:Offset/binary-unit:8,
       Index?INDEX_SIZE_TYPE,
       KeySizeNew?KEY_SIZE_TYPE,
       ValueSizeNew?VALUE_SIZE_TYPE,
       IndexesPart1New/binary>>,
     <<DataPart0:Index/binary-unit:8,
       Key:KeySizeNew/binary-unit:8,
       Value:ValueSizeNew/binary-unit:8,
       0:BlockBufferBitsNew,
       DataPart1/binary>>,
     FreeSizeNew}.

block_size(BytesStored)
    when is_integer(BytesStored) ->
    if
        BytesStored rem ?BLOCK_SIZE_BYTES == 0 ->
            {BytesStored, 0};
        true ->
            BytesTotal = ((BytesStored div ?BLOCK_SIZE_BYTES) + 1) *
                         ?BLOCK_SIZE_BYTES,
            {BytesTotal, (BytesTotal - BytesStored) * 8}
    end.

indexes_adjust(<<>> = Indexes, _) ->
    Indexes;
indexes_adjust(Indexes, 0) ->
    Indexes;
indexes_adjust(Indexes, Difference) ->
    indexes_adjust(<<>>, Indexes, Difference).

indexes_adjust(IndexesNew, <<>>, _) ->
    IndexesNew;
indexes_adjust(IndexesNew,
               <<IndexOld?INDEX_SIZE_TYPE,
                 KeySize?KEY_SIZE_TYPE,
                 ValueSize?VALUE_SIZE_TYPE,
                 IndexesOld/binary>>,
               Difference) ->
    IndexNew = IndexOld + Difference,
    indexes_adjust(<<IndexesNew/binary,
                     IndexNew?INDEX_SIZE_TYPE,
                     KeySize?KEY_SIZE_TYPE,
                     ValueSize?VALUE_SIZE_TYPE>>,
                   IndexesOld,
                   Difference).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

-endif.

