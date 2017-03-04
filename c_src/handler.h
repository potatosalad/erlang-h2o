// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_HANDLER_H
#define H2O_NIF_HANDLER_H

#include "globals.h"

// typedef struct h2o_nif_multi_timer_s h2o_nif_multi_timer_t;

// struct h2o_nif_multi_timer_s {
//     ErlNifTime when;
//     ErlNifPid caller;
//     void (*timeout_function)(h2o_nif_handler_t *handler, ErlNifPid *caller);
//     h2o_nif_multi_timer_t *next;
//     h2o_nif_multi_timer_t *prev;
// };

// #define H2O_NIF_HDL_MAX_ASYNC 1 /* max number of async queue ops */

// typedef struct h2o_nif_async_op_s {
//     int id;           /* id used to identify reply */
//     ErlNifPid caller; /* recipient of async reply */
//     int req;          /* Request id (CONNECT/ACCEPT/RECV) */
//     union {
//         unsigned value; /* Request timeout (since op issued,not started) */
//         h2o_nif_multi_timer_t *mtd;
//     } tmo;
// } h2o_nif_async_op_t;

// typedef struct h2o_nif_async_multi_op_s h2o_nif_async_multi_op_t;

// struct h2o_nif_async_multi_op_s {
//     h2o_nif_async_op_t op;
//     h2o_nif_async_multi_op_t *next;
// };

enum h2o_nif_hdl_type_t { H2O_NIF_HDL_TYPE_NONBLOCK = 0, H2O_NIF_HDL_TYPE_WEBSOCKET };

struct h2o_nif_hdl_child_s {
    h2o_handler_t super;
    h2o_nif_handler_t *parent;
};

struct h2o_nif_handler_s {
    h2o_nif_hdl_type_t type;
    h2o_nif_data_t *priv_data;
    h2o_nif_server_t *server;
    h2o_nif_hdl_child_t *child;
    // ErlNifMutex *mtx;
    ErlNifEnv *env;
    ERL_NIF_TERM tag;
    int state;
    ErlNifPid caller;
    h2o_nif_queue_t acc;
    h2o_nif_queue_t req;
    // h2o_nif_async_op_t *oph; /* queue head or NULL */
    // h2o_nif_async_op_t *opt; /* queue tail or NULL */
    // h2o_nif_async_op_t op_queue[H2O_NIF_HDL_MAX_ASYNC];
    // h2o_nif_async_multi_op_t *multi_first; /* NULL == no multi-accept-queue, op is in ordinary queue */
    // h2o_nif_async_multi_op_t *multi_last;
    // h2o_nif_multi_timer_t *mtd; /* timer structure for multiple accept */
    // int active;
    // int active_count;
    // ERL_NIF_TERM *refs;
    // size_t num_refs;
    // ErlNifPid pid;
};

extern h2o_nif_handler_t *h2o_nif_handler_alloc(ErlNifEnv *env);
extern h2o_nif_hdl_child_t *h2o_nif_handler_register(ErlNifEnv *env, h2o_pathconf_t *pathconf, ERL_NIF_TERM tag,
                                                     h2o_nif_hdl_type_t type);
extern void h2o_nif_handler_dtor(ErlNifEnv *env, void *obj);
extern ERL_NIF_TERM h2o_nif_handler_accept(ErlNifEnv *env, h2o_nif_handler_t *handler, ERL_NIF_TERM timeout);

#endif