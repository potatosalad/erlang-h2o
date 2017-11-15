// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_HANDLER_H
#define H2O_NIF_HANDLER_H

#include "globals.h"
#include "port.h"
#include "server.h"

/* Types */

typedef struct h2o_nif_handler_s h2o_nif_handler_t;
typedef struct h2o_nif_handler_ctx_s h2o_nif_handler_ctx_t;
typedef struct h2o_nif_handler_event_s h2o_nif_handler_event_t;
typedef struct h2o_nif_handler_handle_s h2o_nif_handler_handle_t;

struct h2o_nif_handler_ctx_s {
    h2o_handler_t super;
    _Atomic uintptr_t handler;
};

struct h2o_nif_handler_event_s {
    h2o_nif_port_t super;
    h2o_linklist_t _link;
    h2o_req_t *req;
    _Atomic unsigned long num_async;
    struct {
        ErlNifEnv *env;
        unsigned int status;
        ERL_NIF_TERM headers;
        ErlNifBinary body;
        // int argc;
        // const ERL_NIF_TERM argv[];
    } finalizer;
};

struct h2o_nif_handler_handle_s {
    ERL_NIF_TERM reference;
};

struct h2o_nif_handler_s {
    h2o_nif_port_t super;
    _Atomic uintptr_t ctx;
    atomic_flag state;
    ck_spinlock_t spinlock;
    h2o_linklist_t events;
    _Atomic unsigned long num_events;
};

/* Resource Functions */

static int h2o_nif_handler_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_handler_t **handlerp);
static int h2o_nif_handler_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_handler_event_t **eventp);

inline int
h2o_nif_handler_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_handler_t **handlerp)
{
    assert(handlerp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_HANDLER) {
        *handlerp = NULL;
        return 0;
    }
    *handlerp = (h2o_nif_handler_t *)port;
    return 1;
}

inline int
h2o_nif_handler_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_handler_event_t **eventp)
{
    assert(eventp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_HANDLER_EVENT) {
        if (port == NULL) {
            TRACE_F("port is NULL\n");
        } else {
            TRACE_F("port->type is %d\n", port->type);
        }
        *eventp = NULL;
        return 0;
    }
    *eventp = (h2o_nif_handler_event_t *)port;
    return 1;
}

/* Handler Functions */

extern h2o_nif_handler_ctx_t *h2o_nif_handler_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf,
                                                       h2o_nif_handler_handle_t *hh);

#endif
