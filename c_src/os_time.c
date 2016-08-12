//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// BSD LICENSE
// 
// Copyright (c) 2016, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//

#include <erl_nif.h>
#include <erl_driver.h>
#include <time.h>
#include <errno.h>

#define PREFIX libos_time

#define NIF_NAME_EXPAND(prefix, name) NIF_NAME_EXPAND_I(prefix, name)
#define NIF_NAME_EXPAND_I(prefix, name) NIF_NAME_EXPAND_II(prefix ## _ ## name)
#define NIF_NAME_EXPAND_II(res) res
#define NIF_NAME(name) NIF_NAME_EXPAND(PREFIX, name)
#define NIF_FUNC(name) \
    ERL_NIF_TERM NIF_NAME(name)(ErlNifEnv * env,\
                                int argc,\
                                const ERL_NIF_TERM * argv)
#if ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 8)) || \
    (ERL_NIF_MAJOR_VERSION > 2)
// Erlang/OTP 18.0 and higher
#define DIRTY_SCHEDULERS_VERSION 2
#elif (ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION == 7)
// bypass bug in Erlang/OTP 17.3 release
#define DIRTY_SCHEDULERS_VERSION 1
#else
#define DIRTY_SCHEDULERS_VERSION 0
#endif

#if defined __cplusplus
extern "C"
{
#endif

NIF_FUNC(perf_counter_value)
{
    if (argc != 0)
    {
        return enif_make_badarg(env);
    }
    struct timespec value;
    if (clock_gettime(CLOCK_MONOTONIC, &value) != 0)
    {
        return enif_make_tuple2(env,
                                enif_make_atom(env, "error"),
                                enif_make_atom(env,
                                               erl_errno_id(errno)));
    }
    return enif_make_tuple3(env,
                            enif_make_atom(env, "ok"),
                            enif_make_ulong(env, value.tv_sec),
                            enif_make_ulong(env, value.tv_nsec));
}

static ErlNifFunc nif_funcs[] =
{
#if DIRTY_SCHEDULERS_VERSION == 0
    {"perf_counter_value", 0, NIF_NAME(perf_counter_value)}
#else
    {"perf_counter_value", 0, NIF_NAME(perf_counter_value), 0}
#endif
};
#if defined __cplusplus
}
#endif

#if DIRTY_SCHEDULERS_VERSION == 1
#undef ERL_NIF_INIT
#define ERL_NIF_INIT(NAME, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) \
    ERL_NIF_INIT_PROLOGUE                           \
    ERL_NIF_INIT_GLOB                               \
    ERL_NIF_INIT_DECL(NAME);                        \
    ERL_NIF_INIT_DECL(NAME)                         \
    {                                               \
        static ErlNifEntry entry =                  \
        {                                           \
        ERL_NIF_MAJOR_VERSION,                      \
        ERL_NIF_MINOR_VERSION,                      \
        #NAME,                                      \
        sizeof(FUNCS) / sizeof(*FUNCS),             \
        FUNCS,                                      \
        LOAD, RELOAD, UPGRADE, UNLOAD,              \
        ERL_NIF_VM_VARIANT,                         \
        0                                           \
        };                                          \
        ERL_NIF_INIT_BODY;                          \
        return &entry;                              \
    }                                               \
    ERL_NIF_INIT_EPILOGUE
#endif
ERL_NIF_INIT(os_time, nif_funcs, 0, 0, 0, 0);

