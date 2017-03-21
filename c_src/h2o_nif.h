// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_H
#define H2O_NIF_H

#include "globals.h"

static int h2o_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);
static int h2o_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
static void h2o_nif_unload(ErlNifEnv *env, void *priv_data);

#endif
