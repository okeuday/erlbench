%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    hamt.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-08

-module(hamt).

-export([
	new/0,
	put/3,
	get/2
    ]).

%% testing
-export([
	go/1,
	popcount/1
    ]).

-record(index, {
	bitmask = 0,
	array   = {}
    }).

-record(full, {
	array = {}
    }).

%% defines

-define(node_size_16, 1).

-ifdef(node_size_8).
-define(bitmap_is_full(Bm), Bm =:= 16#FF).
-define(level_shift, 3).
-define(index_mask(Hx), Hx band 7).
-endif.

-ifdef(node_size_16).
-define(bitmap_is_full(Bm), Bm =:= 16#FFFF).
-define(level_shift,4).
-define(index_mask(Hx), Hx band 15).
-endif.

-ifdef(node_size_32).
-define(bitmap_is_full(Bm), Bm =:= 16#FFFFFF).
-define(level_shift,5).
-define(index_mask(Hx), Hx band 31).
-endif.


-define(ielement(Ix, T, V), insert_element(Ix, T, V)).
%-define(ielement(Ix, T, V), erlang:insert_element(Ix, T, V)).
-define(bitpop(Ix,Bm,_),  popcount((Ix) band (Bm))).
%-define(bitpop(Ix,Bm,_), erlang:bitcount( (Ix) band (Bm))).
			    
%% #full{}  = full node
%% #index{} = index node
%% [K|V]    = leaf node

go(N) ->
    lists:foldl(fun(I, O) ->
	    O1 = ?MODULE:put(I, I, O),
	    [io:format("get ~w -> ~w~n", [Ix, ?MODULE:get(Ix, O1)]) || Ix <- lists:seq(1, I)],
	    O1
	end, ?MODULE:new(), lists:seq(1,N)),
    ok.

new() -> #index{ bitmask = 0, array = {} }.

put(K, V, T) ->
    put(erlang:phash2(K), 0, K, V, T).

get(K, T) ->
    get(erlang:phash2(K), K, T).

put(Hx, Lvl, K,V, #index{ bitmask = Bm, array = A} = Ni) ->
    Ix   = ?index_mask(Hx),
    Bp   = 1 bsl Ix,   % bit position
    Slot = ?bitpop(Bm, Bp - 1, Ix) + 1,

    if 
	Bm band Bp > 0 ->
	    % array[Ix] occupied, traverse down
	    Next = case element(Slot, A) of
		[K|_]   -> [K|V];
		[K0|V0] ->
		    Ns = put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, new()),
		    put(erlang:phash2(K0) bsr (Lvl + ?level_shift), Lvl + ?level_shift, K0, V0, Ns);
		Node ->
		    put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, Node)
	    end,

	    Ni#index{ 
		array = setelement(Slot, A, Next)
	    };
	true ->
	    % array[Ix] *not* occupied, set it
	    Bm1 = Bm bor Bp,
	    A1 = ?ielement(Slot, A, [K|V]),
	    if ?bitmap_is_full(Bm1) ->
		    #full { array = A1 };
		true ->
		    #index{ bitmask = Bm1, array = A1 }
	    end
    end;
put(Hx, Lvl, K,V, #full{ array = A} = N) ->
    Ix = ?index_mask(Hx) + 1,
    Next = case element(Ix, A) of
	[K|_]   -> [K|V];
	[K0|V0] ->
	    Ns = put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, new()),
	    put(erlang:phash2(K0) bsr (Lvl + ?level_shift), Lvl + ?level_shift, K0, V0, Ns);
	Node ->
	    put(Hx bsr ?level_shift, Lvl + ?level_shift, K, V, Node)
    end,
    N#full{ array = setelement(Ix, A, Next) }.

get(Hx, K,#index{ bitmask = Bm, array = A}) ->
    Ix = ?index_mask(Hx),
    Bp = 1 bsl Ix,
    if Bm band Bp > 0 ->
	    Slot = ?bitpop(Bm, Bp - 1, Ix) + 1,
	    get(Hx bsr ?level_shift, K, element(Slot, A));
	true -> undefined
    end;
get(Hx, K, #full{ array = A }) ->
    Ix = ?index_mask(Hx) + 1,
    get(Hx bsr ?level_shift, K, element(Ix, A));
get(_, K, [K|V]) -> V;
get(_, _, [_|_]) -> undefined.

% should be bifs

% insert_element(2,{a,b,c},gg) -> {a, gg, b, c}
insert_element(I, A, V) when is_tuple(A), is_integer(I), I > 0 ->
    Vs = tuple_to_list(A),
    list_to_tuple(lelement(I, Vs, V)).

lelement(1, Vs, V) -> [V|Vs];
lelement(I, [V1 | Vs], V) -> [V1 | lelement(I - 1, Vs, V)].

-ifdef(node_size_32).
popcount(X) when is_integer(X) ->
    popcount_byte(X band 255) +
    popcount_byte((X bsr  8) band 255) +
    popcount_byte((X bsr 16) band 255) +
    popcount_byte((X bsr 24) band 255).
-else.
popcount(X) when is_integer(X) ->
    popcount_byte(X band 255) +
    popcount_byte((X bsr  8) band 255).
-endif.

popcount_byte(B) when is_integer(B) ->
    element(B + 1, popcount_byte_table()).

popcount_byte_table() -> {
	0, %   0
	1, %   1
	1, %   2
	2, %   3
	1, %   4
	2, %   5
	2, %   6
	3, %   7
	1, %   8
	2, %   9
	2, %  10
	3, %  11
	2, %  12
	3, %  13
	3, %  14
	4, %  15
	1, %  16
	2, %  17
	2, %  18
	3, %  19
	2, %  20
	3, %  21
	3, %  22
	4, %  23
	2, %  24
	3, %  25
	3, %  26
	4, %  27
	3, %  28
	4, %  29
	4, %  30
	5, %  31
	1, %  32
	2, %  33
	2, %  34
	3, %  35
	2, %  36
	3, %  37
	3, %  38
	4, %  39
	2, %  40
	3, %  41
	3, %  42
	4, %  43
	3, %  44
	4, %  45
	4, %  46
	5, %  47
	2, %  48
	3, %  49
	3, %  50
	4, %  51
	3, %  52
	4, %  53
	4, %  54
	5, %  55
	3, %  56
	4, %  57
	4, %  58
	5, %  59
	4, %  60
	5, %  61
	5, %  62
	6, %  63
	1, %  64
	2, %  65
	2, %  66
	3, %  67
	2, %  68
	3, %  69
	3, %  70
	4, %  71
	2, %  72
	3, %  73
	3, %  74
	4, %  75
	3, %  76
	4, %  77
	4, %  78
	5, %  79
	2, %  80
	3, %  81
	3, %  82
	4, %  83
	3, %  84
	4, %  85
	4, %  86
	5, %  87
	3, %  88
	4, %  89
	4, %  90
	5, %  91
	4, %  92
	5, %  93
	5, %  94
	6, %  95
	2, %  96
	3, %  97
	3, %  98
	4, %  99
	3, % 100
	4, % 101
	4, % 102
	5, % 103
	3, % 104
	4, % 105
	4, % 106
	5, % 107
	4, % 108
	5, % 109
	5, % 110
	6, % 111
	3, % 112
	4, % 113
	4, % 114
	5, % 115
	4, % 116
	5, % 117
	5, % 118
	6, % 119
	4, % 120
	5, % 121
	5, % 122
	6, % 123
	5, % 124
	6, % 125
	6, % 126
	7, % 127
	1, % 128
	2, % 129
	2, % 130
	3, % 131
	2, % 132
	3, % 133
	3, % 134
	4, % 135
	2, % 136
	3, % 137
	3, % 138
	4, % 139
	3, % 140
	4, % 141
	4, % 142
	5, % 143
	2, % 144
	3, % 145
	3, % 146
	4, % 147
	3, % 148
	4, % 149
	4, % 150
	5, % 151
	3, % 152
	4, % 153
	4, % 154
	5, % 155
	4, % 156
	5, % 157
	5, % 158
	6, % 159
	2, % 160
	3, % 161
	3, % 162
	4, % 163
	3, % 164
	4, % 165
	4, % 166
	5, % 167
	3, % 168
	4, % 169
	4, % 170
	5, % 171
	4, % 172
	5, % 173
	5, % 174
	6, % 175
	3, % 176
	4, % 177
	4, % 178
	5, % 179
	4, % 180
	5, % 181
	5, % 182
	6, % 183
	4, % 184
	5, % 185
	5, % 186
	6, % 187
	5, % 188
	6, % 189
	6, % 190
	7, % 191
	2, % 192
	3, % 193
	3, % 194
	4, % 195
	3, % 196
	4, % 197
	4, % 198
	5, % 199
	3, % 200
	4, % 201
	4, % 202
	5, % 203
	4, % 204
	5, % 205
	5, % 206
	6, % 207
	3, % 208
	4, % 209
	4, % 210
	5, % 211
	4, % 212
	5, % 213
	5, % 214
	6, % 215
	4, % 216
	5, % 217
	5, % 218
	6, % 219
	5, % 220
	6, % 221
	6, % 222
	7, % 223
	3, % 224
	4, % 225
	4, % 226
	5, % 227
	4, % 228
	5, % 229
	5, % 230
	6, % 231
	4, % 232
	5, % 233
	5, % 234
	6, % 235
	5, % 236
	6, % 237
	6, % 238
	7, % 239
	4, % 240
	5, % 241
	5, % 242
	6, % 243
	5, % 244
	6, % 245
	6, % 246
	7, % 247
	5, % 248
	6, % 249
	6, % 250
	7, % 251
	6, % 252
	7, % 253
	7, % 254
	8  % 255
    }.
