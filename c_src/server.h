// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_SERVER_H
#define H2O_NIF_SERVER_H

#include "globals.h"
#include "config.h"

#define H2O_NIF_SRV_S_OPEN 0
#define H2O_NIF_SRV_S_IDLE 1
#define H2O_NIF_SRV_S_LOOP 2

// #define H2O_NIF_SRV_S_CLOSED       (0)
// #define H2O_NIF_SRV_S_RUNNING     (H2O_DRV_PORT_F_OPEN)
// #define H2O_NIF_SRV_S_LISTENING    (H2O_DRV_PORT_S_OPEN | H2O_DRV_PORT_F_LISTEN)
// #define H2O_NIF_SRV_S_ACCEPTING    (H2O_DRV_PORT_S_LISTENING | H2O_DRV_PORT_F_ACCEPT)
// #define H2O_NIF_SRV_S_MULTI_ACCEPTING  (H2O_DRV_PORT_S_ACCEPTING | H2O_DRV_PORT_F_MULTI)

// #define H2O_DRV_PORT_IS_OPEN(p) \
//     (((p)->state & H2O_DRV_PORT_F_OPEN) == H2O_DRV_PORT_F_OPEN)

// #define H2O_DRV_PORT_IS_BUSY(p) \
//     (((p)->state & H2O_DRV_PORT_F_BUSY) == H2O_DRV_PORT_F_BUSY)

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

struct h2o_nif_srv_thread_s {
    h2o_nif_server_t *server;
    size_t idx;
    ErlNifTid tid;
    h2o_context_t ctx;
    h2o_multithread_receiver_t server_notifications;
    h2o_multithread_receiver_t memcached;
};

struct h2o_nif_server_s {
    h2o_nif_config_t config;
    uint8_t status;
    time_t launch_time;
    h2o_nif_srv_thread_t *threads;
    volatile sig_atomic_t shutdown_requested;
    volatile sig_atomic_t initialized_threads;
    h2o_nif_srv_state_t state;
};

extern h2o_nif_server_t *h2o_nif_server_alloc(ErlNifEnv *env);
// extern void h2o_nif_server_release(h2o_nif_server_t *server);
extern int h2o_nif_server_init(h2o_nif_server_t *server);
extern ERL_NIF_TERM h2o_nif_server_is_running(ErlNifEnv *env, const ERL_NIF_TERM server_term);
extern void h2o_nif_server_dtor(ErlNifEnv *env, void *obj);
extern int h2o_nif_server_start(ErlNifEnv *env, h2o_nif_server_t *server);

#endif
