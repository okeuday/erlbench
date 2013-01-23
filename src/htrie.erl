%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    htrie.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-08-31

-module(htrie).

-export([
	put/3,
	get/2,
	update/3,
	update/4,
	fullness/1,
	new/0
    ]).

-define(power_of_eight, 1).

% power of two
-ifdef(power_of_four).
-define(node, { nil,nil,nil,nil }). 
-define(mask(X), ((X band 3) + 1)).
-define(shift(X), (X bsr 2)).
-endif.

-ifdef(power_of_eight).
-define(node, { nil,nil,nil,nil, nil,nil,nil,nil }).
-define(mask(X), ((X band 7) + 1)).
-define(shift(X), (X bsr 3)).
-endif.

-ifdef(power_of_sixteen).
-define(node, { 
	nil,nil,nil,nil, 
	nil,nil,nil,nil, 
	nil,nil,nil,nil, 
	nil,nil,nil,nil 
    }).
-define(mask(X), ((X band 15) + 1)).
-define(shift(X), (X bsr 4)).
-endif.

new() -> ?node.

put(K,V,H)      -> do_put(K,V,H,erlang:phash2(K)).
get(K,H)        -> do_get(K,H,erlang:phash2(K)).
update(K,F,I,H) -> do_update(K,F,I,H,erlang:phash2(K)).
update(K,F,H)   -> do_update(K,F,H,erlang:phash2(K)).

do_put(K,V,H,Hx) ->
    Ix = ?mask(Hx),
    case element(Ix, H) of
	nil       -> setelement(Ix, H, [K|V]);
	[K | _]   -> setelement(Ix, H, [K|V]);
	[K0|V0]   -> setelement(Ix, H, {K0,V0, do_put(K,V,?node, ?shift(Hx))});
	{K, _ ,T} -> setelement(Ix, H, {K,V,T});
	{K0,V0,T} -> setelement(Ix, H, {K0,V0, do_put(K,V,T,?shift(Hx))})
    end.

do_get(K,H,Hx) ->
    Ix = ?mask(Hx),
    case element(Ix, H) of
	[K|V]   -> V;
	{K,V,_} -> V;
	{_,_,T} -> do_get(K, T, ?shift(Hx));
	_ ->
	    undefined
    end.

do_update(K,F,H,Hx) ->
    Ix = ?mask(Hx),
    case element(Ix, H) of
	[K|V]   -> setelement(Ix, H, [K|F(V)]);
	{K,V,T} -> setelement(Ix, H, {K,F(V),T});
	E       -> setelement(Ix, H, setelement(3, E, do_update(K,F,element(3,E),?shift(Hx))))
    end.


do_update(K,F,I,H,Hx) ->
    Ix = ?mask(Hx),
    case element(Ix, H) of
	nil       -> setelement(Ix, H, [K|I]);
	[K|V]     -> setelement(Ix, H, [K|F(V)]);
	[K0|V0]   -> setelement(Ix, H, {K0,V0, do_put(K,I,?node, ?shift(Hx))});
	{K,V,T}   -> setelement(Ix, H, {K,F(V),T});
	{K0,V0,T} -> setelement(Ix, H, {K0,V0, do_update(K, F, I, T, ?shift(Hx))})
    end.


fullness(H) -> 
    {A, I} = fullness(H, 1, 0, 0),
    1 - I/(A+I).

fullness(H, Ix, A, I) when Ix < 9 ->
    {A1, I1} = case element(Ix, H) of
	nil      -> {0, 1};
	{_,_,H1} -> fullness(H1, 1, 1, 0);
	_        -> {1, 0}
    end,
    fullness(H, Ix + 1, A + A1, I + I1);
fullness(_, _, A, I) -> {A, I}.


% end impl.

