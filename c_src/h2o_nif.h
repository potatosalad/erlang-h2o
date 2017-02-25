// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_H
#define H2O_NIF_H

#include "globals.h"

static ERL_NIF_TERM h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_server_getstatus_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM h2o_nif_request_reply_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static int h2o_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int h2o_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void h2o_nif_unload(ErlNifEnv *env, void *priv_data);

#endif
