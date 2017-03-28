// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_FILTER_H
#define H2O_NIF_FILTER_H

#include "globals.h"
#include "port.h"
#include "server.h"

/* Types */

typedef struct h2o_nif_filter_s h2o_nif_filter_t;
typedef struct h2o_nif_filter_ctx_s h2o_nif_filter_ctx_t;
typedef struct h2o_nif_filter_handle_s h2o_nif_filter_handle_t;
typedef struct h2o_nif_filter_data_s h2o_nif_filter_data_t;

struct h2o_nif_filter_ctx_s {
    h2o_filter_t super;
    _Atomic uintptr_t filter;
};

struct h2o_nif_filter_handle_s {
    ERL_NIF_TERM reference;
};

struct h2o_nif_filter_s {
    h2o_nif_port_t super;
    _Atomic uintptr_t ctx;
    atomic_flag state;
    ck_spinlock_t spinlock;
    h2o_linklist_t events;
    _Atomic unsigned long num_events;
};

struct h2o_nif_filter_data_s {
    ErlNifEnv *env;
};

/* Resource Functions */

static int h2o_nif_filter_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_t **filterp);

inline int
h2o_nif_filter_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_t **filterp)
{
    assert(filterp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_FILTER) {
        *filterp = NULL;
        return 0;
    }
    *filterp = (h2o_nif_filter_t *)port;
    return 1;
}

/* Filter Functions */

extern h2o_nif_filter_ctx_t *h2o_nif_filter_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf,
                                                     h2o_nif_filter_handle_t *fh);

#endif
