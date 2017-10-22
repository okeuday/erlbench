%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

% Hash Function speed

-module(hash_function).

-export([test/0, test/1, get/2]).

-include("erlbench.hrl").

-define(WORDLIST, "data/words").

crc32_get(String) ->
    erlang:crc32(String).

phash_get(String) ->
    erlang:phash(String, 4294967296).

phash2_get(String) ->
    erlang:phash2(String).

md4_get(String) ->
    <<I:128/big-unsigned-integer>> = crypto:hash(md4, String),
    I.

md5_get(String) ->
    <<I:128/big-unsigned-integer>> = crypto:hash(md5, String),
    I.

ripemd160_get(String) ->
    <<I:160/big-unsigned-integer>> = crypto:hash(ripemd160, String),
    I.

sha_get(String) ->
    <<I:160/big-unsigned-integer>> = crypto:hash(sha, String),
    I.

sha224_get(String) ->
    <<I:224/big-unsigned-integer>> = crypto:hash(sha224, String),
    I.

sha256_get(String) ->
    <<I:256/big-unsigned-integer>> = crypto:hash(sha256, String),
    I.

sha384_get(String) ->
    <<I:384/big-unsigned-integer>> = crypto:hash(sha384, String),
    I.

sha512_get(String) ->
    <<I:512/big-unsigned-integer>> = crypto:hash(sha512, String),
    I.

jenkins_32_get(String) ->
    quickrand_hash:jenkins_32(String).

jenkins_64_get(String) ->
    quickrand_hash:jenkins_64(String).

jenkins64_32_get(String) ->
    quickrand_hash:jenkins64_32(String).

jenkins64_64_get(String) ->
    quickrand_hash:jenkins64_64(String).

jenkins64_128_get(String) ->
    quickrand_hash:jenkins64_128(String).

get(_, []) ->
    ok;

get(Fun, [H | T]) ->
    I = Fun(H),
    true = is_integer(I) andalso (I >= 0),
    get(Fun, T).

test() ->
    test(10000).

test(N) ->
    WordListLines = erlang:min(50000, N),
    Nfinal = N - N rem WordListLines,
    Words = lists:foldl(fun (_, L) ->
        array:to_list(array:resize(WordListLines, read_wordlist())) ++ L
    end, [], lists:seq(WordListLines, Nfinal, WordListLines)),
    true = erlang:length(Words) == Nfinal,
    BWords = [erlang:list_to_binary(Word) || Word <- Words],

    {G1, _} = timer:tc(?MODULE, get, [fun crc32_get/1, Words]),
    {G2, _} = timer:tc(?MODULE, get, [fun phash_get/1, Words]),
    {G3, _} = timer:tc(?MODULE, get, [fun phash2_get/1, Words]),
    {G4, _} = timer:tc(?MODULE, get, [fun md4_get/1, Words]),
    {G5, _} = timer:tc(?MODULE, get, [fun md5_get/1, Words]),
    {G6, _} = timer:tc(?MODULE, get, [fun ripemd160_get/1, Words]),
    {G7, _} = timer:tc(?MODULE, get, [fun sha_get/1, Words]),
    {G8, _} = timer:tc(?MODULE, get, [fun sha224_get/1, Words]),
    {G9, _} = timer:tc(?MODULE, get, [fun sha256_get/1, Words]),
    {G10, _} = timer:tc(?MODULE, get, [fun sha384_get/1, Words]),
    {G11, _} = timer:tc(?MODULE, get, [fun sha512_get/1, Words]),
    {G12, _} = timer:tc(?MODULE, get, [fun jenkins_32_get/1, Words]),
    {G13, _} = timer:tc(?MODULE, get, [fun jenkins_64_get/1, Words]),
    {G14, _} = timer:tc(?MODULE, get, [fun jenkins64_32_get/1, Words]),
    {G15, _} = timer:tc(?MODULE, get, [fun jenkins64_64_get/1, Words]),
    {G16, _} = timer:tc(?MODULE, get, [fun jenkins64_128_get/1, Words]),
    [
        #result{name = "crc32",            get =  G1},
        #result{name = "phash",            get =  G2},
        #result{name = "phash2",           get =  G3},
        #result{name = "md4",              get =  G4},
        #result{name = "md5",              get =  G5},
        #result{name = "ripemd160",        get =  G6},
        #result{name = "sha",              get =  G7},
        #result{name = "sha224",           get =  G8},
        #result{name = "sha256",           get =  G9},
        #result{name = "sha384",           get = G10},
        #result{name = "sha512",           get = G11},
        #result{name = "jenkins_32",       get = G12},
        #result{name = "jenkins_64",       get = G13},
        #result{name = "jenkins64_32",     get = G14},
        #result{name = "jenkins64_64",     get = G15},
        #result{name = "jenkins64_128",    get = G16}%,
    ].

read_wordlist() ->
    {ok, F} = file:open(?WORDLIST, [read_ahead, raw, read]),
    Array = array:new([{size, 524288}, {default, -1}, {fixed, false}]),
    shuffle:shuffle(read_wordlist(0, F, Array)).

read_wordlist(I, F, Array) ->
    case file:read_line(F) of
        {ok, Line} ->
            Word = lists:sublist(Line, erlang:length(Line) - 1),
            if
                Word == "" ->
                    read_wordlist(I, F, Array);
                true ->
                    read_wordlist(I + 1, F, array:set(I, Word, Array))
            end;
        eof ->
            array:fix(array:resize(I, Array))
    end.

