// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_SERVER_H
#define H2O_NIF_SERVER_H

#include "globals.h"
#include "port.h"
#include "config.h"
#include "ipc.h"

/* Types */

typedef struct h2o_nif_server_s h2o_nif_server_t;
typedef struct h2o_nif_srv_listen_s h2o_nif_srv_listen_t;
typedef struct h2o_nif_srv_thread_s h2o_nif_srv_thread_t;
typedef struct h2o_nif_srv_thread_ctx_s h2o_nif_srv_thread_ctx_t;

struct h2o_nif_srv_listen_s {
    h2o_nif_srv_thread_t *thread;
    h2o_accept_ctx_t accept_ctx;
    h2o_socket_t *sock;
};

struct h2o_nif_srv_thread_ctx_s {
    h2o_context_t super;
    h2o_nif_srv_thread_t *thread;
};

struct h2o_nif_srv_thread_s {
    h2o_nif_server_t *server;
    size_t idx;
    ErlNifTid tid;
    h2o_nif_srv_thread_ctx_t ctx;
    h2o_multithread_receiver_t server_notifications;
    h2o_multithread_receiver_t memcached;
    // h2o_multithread_receiver_t erlang;
    h2o_nif_ipc_queue_t *ipc_queue;
};

struct h2o_nif_server_s {
    h2o_nif_port_t super;
    h2o_nif_config_t config;
    time_t launch_time;
    h2o_nif_srv_thread_t *threads;
    _Atomic int shutdown_requested;
    _Atomic size_t initialized_threads;
    _Atomic size_t shutdown_threads;
    struct {
        /* unused buffers exist to avoid false sharing of the cache line */
        char _unused1_avoir_false_sharing[32];
        _Atomic int
            _num_connections; /* number of currently handled incoming connections, should use atomic functions to update the value
                               */
        char _unused2_avoir_false_sharing[32];
        _Atomic unsigned long
            _num_sessions; /* total number of opened incoming connections, should use atomic functions to update the value */
        char _unused3_avoir_false_sharing[32];
    } state;
};

/* Resource Functions */

static int h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_server_t **serverp);

inline int
h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_server_t **serverp)
{
    assert(serverp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_SERVER) {
        *serverp = NULL;
        return 0;
    }
    *serverp = (h2o_nif_server_t *)port;
    return 1;
}

/* Port Functions */

extern int h2o_nif_server_open(h2o_nif_server_t **serverp);

/* Server Functions */

extern int h2o_nif_server_start(h2o_nif_server_t *server);
static int h2o_nif_ipc_request(h2o_req_t *req, h2o_nif_ipc_callback_t *callback, void *data);

inline int
h2o_nif_ipc_request(h2o_req_t *req, h2o_nif_ipc_callback_t *callback, void *data)
{
    h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)req->conn->ctx;
    return h2o_nif_ipc_send(ctx->thread->ipc_queue, callback, data);
}

#endif
