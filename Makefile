#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
ERL_OPTS=+C multi_time_warp
COMPILE_FLAGS=+nowarn_unused_function

all: test

test: beam
	erl $(ERL_OPTS) -noshell -pz ebin -s run test -s init stop

clean:
	rm -f ebin/*.beam
	rm -f c_src/*.o
	rm -f priv/*.so

BUILT=\
    ebin/aadict.beam \
    ebin/binary_key.beam \
    ebin/blookupf.beam \
    ebin/blookupv.beam \
    ebin/btree7.beam \
    ebin/btrie.beam \
    ebin/ejson_decode.beam \
    ebin/ejson_encode.beam \
    ebin/ejson.beam \
    ebin/hamt.beam \
    ebin/hash_function.beam \
    ebin/hashdict.beam \
    ebin/hasht.beam \
    ebin/hashtl2.beam \
    ebin/hashtl3.beam \
    ebin/hashtl4.beam \
    ebin/hashtl.beam \
    ebin/htrie.beam \
    ebin/integer_key.beam \
    ebin/json_decode.beam \
    ebin/json_encode.beam \
    ebin/jsx_config.beam \
    ebin/jsx_decoder.beam \
    ebin/jsx_encoder.beam \
    ebin/jsx.beam \
    ebin/jsx_parser.beam \
    ebin/jsx_to_json.beam \
    ebin/jsx_to_term.beam \
    ebin/jsx_verify.beam \
    ebin/list_match.beam \
    ebin/list_traversal.beam \
    ebin/math_speed.beam \
    ebin/mochijson2.beam \
    ebin/mochinum.beam \
    ebin/nicefloats.beam \
    ebin/ntree.beam \
    ebin/os_time.beam \
    ebin/pseudo_randomness.beam \
    ebin/pqueue.beam \
    ebin/pqueue2.beam \
    ebin/pqueue3.beam \
    ebin/pqueue4.beam \
    ebin/pqueue_priority0.beam \
    ebin/pqueue_priorities2.beam \
    ebin/pqueue_priorities41.beam \
    ebin/pqueue_priorities64.beam \
    ebin/priority_queue.beam \
    ebin/queue_in_out.beam \
    ebin/quickrand.beam \
    ebin/quickrand_cache.beam \
    ebin/quickrand_cache_normal.beam \
    ebin/quickrand_hash.beam \
    ebin/quickrand_normal.beam \
    ebin/random_wh06_int.beam \
    ebin/random_wh82.beam \
    ebin/random_wh82_int.beam \
    ebin/rbdict.beam \
    ebin/rfc4627.beam \
    ebin/run.beam \
    ebin/shuffle.beam \
    ebin/string_key.beam \
    ebin/trie.beam \
    ebin/trie_prefix.beam \
    ebin/ttdict.beam \
    ebin/uuid_creation.beam \
    ebin/uuid.beam

beam: $(BUILT) priv/libos_time.so

priv/libos_time.so: c_src/os_time.o
	@mkdir -p priv
	gcc -shared -o $@ $<

c_src/%.o: c_src/%.c
	gcc -c -fPIC -o $@ $<

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc $(COMPILE_FLAGS) -o ebin/ $<
