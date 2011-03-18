
all: test

test: beam
	erl -noshell -pz ebin -s run test -s init stop

BUILT=\
    ebin/aadict.beam \
    ebin/ejson_decode.beam \
    ebin/ejson_encode.beam \
    ebin/ejson.beam \
    ebin/hasht.beam \
    ebin/hashtl2.beam \
    ebin/hashtl3.beam \
    ebin/hashtl4.beam \
    ebin/hashtl.beam \
    ebin/integer_key.beam \
    ebin/json_decode.beam \
    ebin/json_encode.beam \
    ebin/jsx_eep0018.beam \
    ebin/jsx.beam \
    ebin/jsx_format.beam \
    ebin/jsx_utf16.beam \
    ebin/jsx_utf16le.beam \
    ebin/jsx_utf32.beam \
    ebin/jsx_utf32le.beam \
    ebin/jsx_utf8.beam \
    ebin/jsx_verify.beam \
    ebin/list_traversal.beam \
    ebin/lqueue.beam \
    ebin/mochijson2.beam \
    ebin/mochinum.beam \
    ebin/nicefloats.beam \
    ebin/queue_in_out.beam \
    ebin/rbdict.beam \
    ebin/rfc4627.beam \
    ebin/run.beam \
    ebin/string_key.beam \
    ebin/trie.beam

beam: $(BUILT)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -o ebin/ $<
