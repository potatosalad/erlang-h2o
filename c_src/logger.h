// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_LOGGER_H
#define H2O_NIF_LOGGER_H

#include "globals.h"
#include "port.h"
#include "server.h"

#define H2O_NIF_LOGGER_STATE_PASSIVE (0)
#define H2O_NIF_LOGGER_STATE_NOTIFY (1)
#define H2O_NIF_LOGGER_STATE_ACTIVE (2)

/* Types */

typedef struct h2o_nif_logger_s h2o_nif_logger_t;
typedef struct h2o_nif_logger_ctx_s h2o_nif_logger_ctx_t;
typedef struct h2o_nif_logger_event_s h2o_nif_logger_event_t;
typedef struct h2o_nif_logger_handle_s h2o_nif_logger_handle_t;

struct h2o_nif_logger_ctx_s {
    h2o_logger_t super;
    _Atomic uintptr_t logger;
};

struct h2o_nif_logger_event_s {
    h2o_linklist_t _link;
    ErlNifBinary binary;
};

struct h2o_nif_logger_handle_s {
    h2o_logconf_t *logconf;
    ERL_NIF_TERM reference;
};

struct h2o_nif_logger_s {
    h2o_nif_port_t super;
    _Atomic uintptr_t ctx;
    h2o_nif_logger_handle_t *lh;
    _Atomic int state;
    ck_spinlock_t spinlock;
    h2o_linklist_t events;
    _Atomic unsigned long num_events;
};

/* Resource Functions */

static int h2o_nif_logger_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_logger_t **loggerp);

inline int
h2o_nif_logger_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_logger_t **loggerp)
{
    assert(loggerp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_LOGGER) {
        *loggerp = NULL;
        return 0;
    }
    *loggerp = (h2o_nif_logger_t *)port;
    return 1;
}

/* Logger Functions */

extern h2o_nif_logger_ctx_t *h2o_nif_logger_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh);

#endif
