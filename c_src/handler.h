// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_HANDLER_H
#define H2O_NIF_HANDLER_H

#include "globals.h"

struct h2o_nif_hdl_child_s {
    h2o_handler_t super;
    h2o_nif_handler_t *parent;
};

struct h2o_nif_handler_s {
    h2o_nif_data_t *priv_data;
    h2o_nif_server_t *server;
    h2o_nif_hdl_child_t *child;
    ErlNifMutex *mtx;
    ErlNifEnv *env;
    ERL_NIF_TERM tag;
    ERL_NIF_TERM *refs;
    size_t num_refs;
    ErlNifPid pid;
};

extern h2o_nif_handler_t *h2o_nif_handler_alloc(ErlNifEnv *env);
extern h2o_nif_hdl_child_t *h2o_nif_handler_register(ErlNifEnv *env, h2o_pathconf_t *pathconf, ERL_NIF_TERM tag);
extern void h2o_nif_handler_dtor(ErlNifEnv *env, void *obj);

#endif