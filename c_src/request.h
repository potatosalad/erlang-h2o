// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_REQUEST_H
#define H2O_NIF_REQUEST_H

#include "globals.h"
#include "handler.h"
#include "server.h"

#include "resource.h"

typedef int H2O_NIF_REQUEST;

#define H2O_NIF_REQUEST_HTTP 0
#define H2O_NIF_REQUEST_WEBSOCKET 1

typedef struct h2o_nif_multithread_request_s h2o_nif_multithread_request_t;

typedef struct h2o_nif_multithread_request_args_s h2o_nif_multithread_request_args_t;

typedef void h2o_nif_multithread_request_callback_t(h2o_nif_request_t *request, h2o_nif_multithread_request_t *ctx);

struct h2o_nif_multithread_request_s {
    h2o_multithread_message_t super;
    h2o_nif_multithread_request_callback_t *callback;
    h2o_nif_request_t *request;
};

struct h2o_nif_multithread_request_args_s {
    h2o_nif_multithread_request_t super;
    ErlNifEnv *env;
    int argc;
    ERL_NIF_TERM argv[10];
};

struct h2o_nif_request_s {
    h2o_nif_resource_t resource;
    _Atomic int finalizer;
    H2O_NIF_REQUEST type;
    h2o_nif_port_t *port;
    h2o_nif_handler_t *handler;
    h2o_req_t *req;
};

extern int h2o_nif_request_accept_http(h2o_nif_port_t *parent, h2o_nif_handler_t *handler, h2o_req_t *req, h2o_nif_port_t **portp);
static int h2o_nif_request_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp, h2o_nif_request_t **requestp);
static void h2o_nif_request_wakeup(h2o_nif_request_t *request);
static int h2o_nif_request_begin(h2o_nif_request_t *request);
static int h2o_nif_request_end(h2o_nif_request_t *request);
static int h2o_nif_request_finish(h2o_nif_request_t *request);
// extern h2o_nif_request_t *h2o_nif_request_alloc(h2o_nif_handler_t *handler);
// extern h2o_nif_request_t *h2o_nif_request_create(h2o_nif_handler_t *handler, h2o_req_t *req);
// extern int h2o_nif_request_dispatch(h2o_nif_request_t *request);
// extern void h2o_nif_request_dtor(ErlNifEnv *env, void *obj);
// extern void h2o_nif_request_on_ws_message(h2o_websocket_conn_t *conn, const struct wslay_event_on_msg_recv_arg *arg);

inline int
h2o_nif_request_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp, h2o_nif_request_t **requestp)
{
    assert(portp != NULL);
    assert(requestp != NULL);
    h2o_nif_port_t *port = NULL;
    *portp = NULL;
    *requestp = NULL;
    if (!h2o_nif_port_get(env, id, &port)) {
        return 0;
    }
    if (port->type != H2O_NIF_PORT_TYPE_REQUEST) {
        (void)h2o_nif_port_release(port);
        return 0;
    }
    *portp = port;
    *requestp = (h2o_nif_request_t *)port->data;
    return 1;
}

inline void
h2o_nif_request_wakeup(h2o_nif_request_t *request)
{
    h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
    (void)h2o_multithread_send_message(&ctx->thread->server_notifications, NULL);
}

inline int
h2o_nif_request_begin(h2o_nif_request_t *request)
{
    int expected;
    int desired;
    int retval;
    do {
        expected = atomic_load_explicit(&request->finalizer, memory_order_relaxed);
        if (expected == 0 || h2o_nif_port_is_closed(request->port)) {
            return 0;
        }
        desired = expected + 1;
        retval = atomic_compare_exchange_weak_explicit(&request->finalizer, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!retval) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    return retval;
}

inline int
h2o_nif_request_end(h2o_nif_request_t *request)
{
    (void)atomic_fetch_sub_explicit(&request->finalizer, 1, memory_order_relaxed);
    return 1;
}

inline int
h2o_nif_request_finish(h2o_nif_request_t *request)
{
    if (!h2o_nif_port_finish(request->port)) {
        return 0;
    }
    (void)h2o_nif_request_end(request);
    ck_backoff_t backoff = CK_BACKOFF_INITIALIZER;
    while (atomic_load_explicit(&request->finalizer, memory_order_relaxed) != 0) {
        (void)ck_backoff_eb(&backoff);
    }
    return 1;
}

static int h2o_nif_request_keep(h2o_nif_request_t *request);
static int h2o_nif_request_release(h2o_nif_request_t *request);

inline int
h2o_nif_request_keep(h2o_nif_request_t *request)
{
    return h2o_nif_resource_keep((void *)request);
}

inline int
h2o_nif_request_release(h2o_nif_request_t *request)
{
    return h2o_nif_resource_release((void *)request);
}

#endif