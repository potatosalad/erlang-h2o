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
// typedef struct h2o_nif_handler_event_generator_s h2o_nif_handler_event_generator_t;

struct h2o_nif_handler_ctx_s {
    h2o_handler_t super;
    _Atomic uintptr_t handler;
};

struct h2o_nif_handler_event_s {
    h2o_nif_port_t super;
    h2o_linklist_t _link;
    h2o_req_t *req;
    _Atomic unsigned long num_async;
    _Atomic size_t entity_offset;
    // _Atomic h2o_nif_handler_event_generator_t *generator;
    struct {
        ErlNifEnv *env;
        unsigned int status;
        ERL_NIF_TERM headers;
        ErlNifBinary body;
    } finalizer;
};

// struct h2o_nif_handler_event_generator_s {
//     h2o_generator_t super;
//     h2o_nif_handler_event_t *event;
//     h2o_doublebuffer_t sending;
// };

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
extern ERL_NIF_TERM h2o_nif_handler_event_make(ErlNifEnv *env, h2o_nif_handler_event_t *event);

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

/* IPC Functions */

typedef struct h2o_nif_ipc_handler_event_s h2o_nif_ipc_handler_event_t;

struct h2o_nif_ipc_handler_event_s {
    h2o_nif_ipc_message_t super;
    h2o_nif_handler_event_t *event;
    void *arg0;
    void *arg1;
    void *arg2;
};

static int h2o_nif_ipc_enqueue_handler_event(h2o_nif_handler_event_t *event, h2o_nif_ipc_callback_t *cb);
static int h2o_nif_ipc_enqueue_handler_event_1(h2o_nif_handler_event_t *event, void *arg0, h2o_nif_ipc_callback_t *cb);
static int h2o_nif_ipc_enqueue_handler_event_2(h2o_nif_handler_event_t *event, void *arg0, void *arg1, h2o_nif_ipc_callback_t *cb);
static int h2o_nif_ipc_enqueue_handler_event_3(h2o_nif_handler_event_t *event, void *arg0, void *arg1, void *arg2,
                                               h2o_nif_ipc_callback_t *cb);

inline int
h2o_nif_ipc_enqueue_handler_event(h2o_nif_handler_event_t *event, h2o_nif_ipc_callback_t *cb)
{
    return h2o_nif_ipc_enqueue_handler_event_3(event, NULL, NULL, NULL, cb);
}

inline int
h2o_nif_ipc_enqueue_handler_event_1(h2o_nif_handler_event_t *event, void *arg0, h2o_nif_ipc_callback_t *cb)
{
    return h2o_nif_ipc_enqueue_handler_event_3(event, arg0, NULL, NULL, cb);
}

inline int
h2o_nif_ipc_enqueue_handler_event_2(h2o_nif_handler_event_t *event, void *arg0, void *arg1, h2o_nif_ipc_callback_t *cb)
{
    return h2o_nif_ipc_enqueue_handler_event_3(event, arg0, arg1, NULL, cb);
}

inline int
h2o_nif_ipc_enqueue_handler_event_3(h2o_nif_handler_event_t *event, void *arg0, void *arg1, void *arg2, h2o_nif_ipc_callback_t *cb)
{
    h2o_nif_ipc_handler_event_t *message = (void *)h2o_nif_ipc_create_message(sizeof(*message), cb, NULL);
    h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)event->req->conn->ctx;
    message->event = event;
    message->arg0 = arg0;
    message->arg1 = arg1;
    message->arg2 = arg2;
    return h2o_nif_ipc_enqueue(ctx->thread->ipc_queue, (h2o_nif_ipc_message_t *)message);
}

// inline int
// h2o_nif_ipc_enqueue_handler_event(h2o_nif_handler_event_t *event, size_t size, h2o_nif_ipc_callback_t *callback)
// {
//     assert(size >= sizeof(h2o_nif_ipc_handler_event_t));
//     h2o_nif_ipc_handler_event_t *message = enif_alloc(sizeof(*message));
//     h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)event->req->conn->ctx;
//     message->super._link.prev = message->super._link.next = NULL;
//     message->super.callback = callback;
//     message->event = event;
//     return h2o_nif_ipc_enqueue(ctx->thread->ipc_queue, (h2o_nif_ipc_message_t *)message);
// }

#endif
