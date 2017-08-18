// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_BATCH_H
#define H2O_NIF_BATCH_H

#include "globals.h"
// #include "ipc.h"
// #include "handler.h"

#define H2O_NIF_BATCH_MAX_ARITY 4

/* Types */

typedef struct h2o_nif_batch_s h2o_nif_batch_t;
typedef struct h2o_nif_batch_fun_s h2o_nif_batch_fun_t;
typedef struct h2o_nif_batch_req_s h2o_nif_batch_req_t;
typedef struct h2o_nif_batch_ctx_s h2o_nif_batch_ctx_t;
// typedef int h2o_nif_batch_test_cb_t(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
typedef int h2o_nif_batch_req_cb_t(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// typedef struct h2o_nif_ipc_batch_req_s h2o_nif_ipc_batch_req_t;

struct h2o_nif_batch_fun_s {
    h2o_nif_batch_req_cb_t *test;
    h2o_nif_batch_req_cb_t *exec;
    h2o_nif_batch_req_cb_t *done;
};

struct h2o_nif_batch_req_s {
    h2o_nif_batch_req_t *next;
    h2o_nif_batch_fun_t *fun;
    _Atomic int refc;
    struct {
        ErlNifPid to;
        ERL_NIF_TERM tag;
    } from;
    int argc;
    ERL_NIF_TERM argv[H2O_NIF_BATCH_MAX_ARITY];
};

struct h2o_nif_batch_s {
    size_t max_per_slice;
    size_t length;
    size_t offset;
    size_t num_req;
    ErlNifEnv *env;
    h2o_nif_batch_req_t *req;
};

struct h2o_nif_batch_ctx_s {
    h2o_nif_batch_t *batch;
    h2o_nif_batch_req_t *req;
};

// struct h2o_nif_ipc_batch_req_s {
//     h2o_nif_ipc_message_t super;
//     h2o_nif_batch_t *batch;
//     h2o_nif_batch_req_t *req;
//     union {
//         h2o_nif_handler_event_t *handler_event;
//     } data;
// };

/* Variables */

extern ErlNifResourceType *h2o_nif_batch_resource_type;

/* NIF Functions */

extern int h2o_nif_batch_load(ErlNifEnv *env, h2o_nif_data_t *nif_data);
extern int h2o_nif_batch_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
extern void h2o_nif_batch_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data);

/* Resource Functions */

extern int h2o_nif_batch_create(ErlNifEnv *env, ERL_NIF_TERM list, h2o_nif_batch_t **batchp);
static int h2o_nif_batch_get(ErlNifEnv *env, ERL_NIF_TERM batch_term, h2o_nif_batch_t **batchp);
static void h2o_nif_batch_keep(h2o_nif_batch_t *batch);
static ERL_NIF_TERM h2o_nif_batch_make(ErlNifEnv *env, h2o_nif_batch_t *batch);
static void h2o_nif_batch_release(h2o_nif_batch_t *batch);

inline int
h2o_nif_batch_get(ErlNifEnv *env, ERL_NIF_TERM batch_term, h2o_nif_batch_t **batchp)
{
    assert(batchp != NULL);
    h2o_nif_batch_t *batch = NULL;
    if (!enif_get_resource(env, batch_term, h2o_nif_batch_resource_type, (void **)&batch)) {
        *batchp = NULL;
        return 0;
    }
    *batchp = batch;
    return 1;
}

inline void
h2o_nif_batch_keep(h2o_nif_batch_t *batch)
{
    (void)enif_keep_resource((void *)batch);
}

inline ERL_NIF_TERM
h2o_nif_batch_make(ErlNifEnv *env, h2o_nif_batch_t *batch)
{
    return enif_make_resource(env, (void *)batch);
}

inline void
h2o_nif_batch_release(h2o_nif_batch_t *batch)
{
    (void)enif_release_resource((void *)batch);
}

/* Batch Functions */

extern ERL_NIF_TERM h2o_nif_batch_process(h2o_nif_batch_t *batch, ErlNifEnv *env, ERL_NIF_TERM *listp);
extern ERL_NIF_TERM h2o_nif_batch_execute(h2o_nif_batch_t *batch, ErlNifEnv *env);
extern ERL_NIF_TERM h2o_nif_batch_resolve(h2o_nif_batch_t *batch, ErlNifEnv *env);
// static int h2o_nif_ipc_batch_handler_event(h2o_nif_handler_event_t *handler_event, h2o_nif_batch_t *batch, h2o_nif_batch_req_t
// *req,
//                                            h2o_nif_ipc_callback_t *cb);

// inline int
// h2o_nif_ipc_batch_handler_event(h2o_nif_handler_event_t *handler_event, h2o_nif_batch_t *batch, h2o_nif_batch_req_t *req,
//                                 h2o_nif_ipc_callback_t *cb)
// {
//     h2o_nif_ipc_batch_req_t *message = (void *)h2o_nif_ipc_create_message(sizeof(*message), cb, NULL);
//     h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)handler_event->req->conn->ctx;
//     (void)atomic_fetch_add_explicit(&req->refc, 1, memory_order_relaxed);
//     message->batch = batch;
//     message->req = req;
//     message->data.handler_event = handler_event;
//     return h2o_nif_ipc_enqueue(ctx->thread->ipc_queue, (h2o_nif_ipc_message_t *)message);
// }

#endif
