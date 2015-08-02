#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
ERL_OPTS=+C multi_time_warp
COMPILE_FLAGS=-DERLANG_OTP_VERSION_18
COMPILE_FLAGS_HIPE=\
icode_range,icode_ssa_const_prop,\
icode_ssa_copy_prop,icode_type,icode_inline_bifs,\
rtl_lcm,rtl_ssa,rtl_ssa_const_prop,spillmin_color,\
use_indexing,remove_comments,concurrent_comp,\
binary_opt,inline_fp,pmatch,peephole,verbose,verbose
COMPILE_FLAGS_O0=
COMPILE_FLAGS_O1=\
+warn_obsolete_guard \
+warn_unused_import \
+warn_shadow_vars \
+warn_export_vars \
+debug_info \
+native \
+'{hipe,[o1,verbose,$(COMPILE_FLAGS_HIPE)]}'
COMPILE_FLAGS_O2=\
+warn_obsolete_guard \
+warn_unused_import \
+warn_shadow_vars \
+warn_export_vars \
+debug_info \
+native \
+'{hipe,[o2,verbose,$(COMPILE_FLAGS_HIPE)]}'
COMPILE_FLAGS_O3=\
+warn_obsolete_guard \
+warn_unused_import \
+warn_shadow_vars \
+warn_export_vars \
+debug_info \
+native \
+'{hipe,[o3,verbose,$(COMPILE_FLAGS_HIPE)]}'
COMPILE_FLAGS_O3_INLINE=\
+warn_obsolete_guard \
+warn_unused_import \
+warn_shadow_vars \
+warn_export_vars \
+debug_info \
+native \
+'{hipe,[o3,verbose,$(COMPILE_FLAGS_HIPE)]}' \
+inline \
+inline_list_funcs

ifeq ($(OPTIMIZE),4)
COMPILE_FLAGS+=$(COMPILE_FLAGS_O3_INLINE)
else
ifeq ($(OPTIMIZE),3)
COMPILE_FLAGS+=$(COMPILE_FLAGS_O3)
else
ifeq ($(OPTIMIZE),2)
COMPILE_FLAGS+=$(COMPILE_FLAGS_O2)
else
ifeq ($(OPTIMIZE),1)
COMPILE_FLAGS+=$(COMPILE_FLAGS_O1)
else
$(warning OPTIMIZE defaults to 0 (no compile options))
COMPILE_FLAGS+=$(COMPILE_FLAGS_O0)
endif
endif
endif
endif

all: test

test: beam
	erl $(ERL_OPTS) -noshell -pz ebin -s run test -s init stop

clean:
	rm -f ebin/*.beam

BUILT=\
    ebin/aadict.beam \
    ebin/binary_key.beam \
    ebin/btree7.beam \
    ebin/btrie.beam \
    ebin/ejson_decode.beam \
    ebin/ejson_encode.beam \
    ebin/ejson.beam \
    ebin/hamt.beam \
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
    ebin/lqueue.beam \
    ebin/mochijson2.beam \
    ebin/mochinum.beam \
    ebin/nicefloats.beam \
    ebin/ntree.beam \
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
    ebin/random_wh06_int.beam \
    ebin/rbdict.beam \
    ebin/rfc4627.beam \
    ebin/run.beam \
    ebin/shuffle.beam \
    ebin/string_key.beam \
    ebin/trie.beam \
    ebin/trie_prefix.beam \
    ebin/uuid_creation.beam \
    ebin/uuid.beam

beam: $(BUILT)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc $(COMPILE_FLAGS) -o ebin/ $<
