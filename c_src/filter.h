// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_FILTER_H
#define H2O_NIF_FILTER_H

#include "globals.h"
#include "port.h"
#include "server.h"

/* Types */

typedef struct h2o_nif_filter_s h2o_nif_filter_t;
typedef struct h2o_nif_filter_handle_s h2o_nif_filter_handle_t;

struct h2o_nif_filter_s {
    h2o_nif_port_t super;
    struct {
        ck_spinlock_t lock;
        atomic_flag ready_input;
        h2o_linklist_t input;
        _Atomic size_t size;
    } events;
};

struct h2o_nif_filter_handle_s {
    ERL_NIF_TERM reference;
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

/* Config Functions */

extern int h2o_nif_filter_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_handle_t *fh);

#endif
