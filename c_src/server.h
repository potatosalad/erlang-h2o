// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_SERVER_H
#define H2O_NIF_SERVER_H

#include "globals.h"
#include "config.h"
#include "port.h"
#include "ipc.h"

#include "resource.h"

struct h2o_nif_srv_listen_s {
    h2o_nif_srv_thread_t *thread;
    h2o_accept_ctx_t accept_ctx;
    h2o_socket_t *sock;
};

struct h2o_nif_srv_state_s {
    /* unused buffers exist to avoid false sharing of the cache line */
    char _unused1_avoir_false_sharing[32];
    int _num_connections; /* number of currently handled incoming connections, should use atomic functions to update the value */
    char _unused2_avoir_false_sharing[32];
    unsigned long _num_sessions; /* total number of opened incoming connections, should use atomic functions to update the value */
    char _unused3_avoir_false_sharing[32];
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
    h2o_multithread_receiver_t erlang;
    h2o_nif_ipc_queue_t *ipc_queue;
};

struct h2o_nif_server_s {
    h2o_nif_resource_t resource;
    h2o_nif_config_t config;
    h2o_nif_port_t *port;
    // uint8_t status;
    time_t launch_time;
    h2o_nif_srv_thread_t *threads;
    volatile sig_atomic_t shutdown_requested;
    volatile sig_atomic_t initialized_threads;
    volatile sig_atomic_t shutdown_threads;
    h2o_nif_srv_state_t state;
};

// extern h2o_nif_server_t *h2o_nif_server_alloc(ErlNifEnv *env);
extern int h2o_nif_server_open(h2o_nif_port_t **portp);
static int h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp, h2o_nif_server_t **serverp);
extern int h2o_nif_server_start(h2o_nif_port_t *port, h2o_nif_server_t *server);
// extern int h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_server_t **serverp);
// extern void h2o_nif_server_release(h2o_nif_server_t *server);
// extern int h2o_nif_server_init(h2o_nif_server_t *server);
// extern ERL_NIF_TERM h2o_nif_server_is_running(ErlNifEnv *env, const ERL_NIF_TERM server_term);
// extern void h2o_nif_server_on_close(ErlNifEnv *env, h2o_nif_port_t *port);
// extern void h2o_nif_server_dtor(ErlNifEnv *env, void *obj);
// extern int h2o_nif_server_start(ErlNifEnv *env, h2o_nif_server_t *server);
// extern h2o_iovec_t h2o_nif_server_on_extra_status(void *unused, h2o_globalconf_t *_conf, h2o_req_t *req);

inline int
h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp, h2o_nif_server_t **serverp)
{
    assert(portp != NULL);
    assert(serverp != NULL);
    h2o_nif_port_t *port = NULL;
    *portp = NULL;
    *serverp = NULL;
    if (!h2o_nif_port_get(env, id, &port)) {
        return 0;
    }
    if (port->type != H2O_NIF_PORT_TYPE_SERVER) {
        (void)h2o_nif_port_release(port);
        return 0;
    }
    *portp = port;
    *serverp = (h2o_nif_server_t *)port->data;
    return 1;
}

static int h2o_nif_server_keep(h2o_nif_server_t *server);
static int h2o_nif_server_release(h2o_nif_server_t *server);

inline int
h2o_nif_server_keep(h2o_nif_server_t *server)
{
    return h2o_nif_resource_keep((void *)server);
}

inline int
h2o_nif_server_release(h2o_nif_server_t *server)
{
    return h2o_nif_resource_release((void *)server);
}

#endif
