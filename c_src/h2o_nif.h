// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_H
#define H2O_NIF_H

#include "globals.h"

#ifndef timersub
#define timersub(tvp, uvp, vvp)                                                                                                    \
    do {                                                                                                                           \
        (vvp)->tv_sec = (tvp)->tv_sec - (uvp)->tv_sec;                                                                             \
        (vvp)->tv_usec = (tvp)->tv_usec - (uvp)->tv_usec;                                                                          \
        if ((vvp)->tv_usec < 0) {                                                                                                  \
            (vvp)->tv_sec--;                                                                                                       \
            (vvp)->tv_usec += 1000000;                                                                                             \
        }                                                                                                                          \
    } while ((vvp)->tv_usec >= 1000000)
#endif

#define MAX_PER_SLICE 20000 // 20 KB

// static ERL_NIF_TERM h2o_nif_atom_check_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// static ERL_NIF_TERM h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_server_getstatus_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// static ERL_NIF_TERM h2o_nif_handler_accept_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// static ERL_NIF_TERM h2o_nif_request_reply_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_is_websocket_handshake_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_upgrade_to_websocket_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_delegate_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_send_inline_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_set_status_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static ERL_NIF_TERM h2o_nif_request_add_header_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static int h2o_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int h2o_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void h2o_nif_unload(ErlNifEnv *env, void *priv_data);

#endif
