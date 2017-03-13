// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_HANDLER_H
#define H2O_NIF_HANDLER_H

#include "globals.h"
#include "port.h"
#include "server.h"

#include "resource.h"

typedef int H2O_NIF_HANDLER;

#define H2O_NIF_HANDLER_HTTP 0
#define H2O_NIF_HANDLER_WEBSOCKET 1

struct h2o_nif_handler_ctx_s {
    h2o_handler_t super;
    h2o_nif_handler_t *handler;
};

struct h2o_nif_handler_s {
    h2o_nif_resource_t resource;
    H2O_NIF_HANDLER type;
    h2o_nif_port_t *port;
    h2o_nif_server_t *server;
    h2o_nif_handler_ctx_t *ctx;
};

typedef struct h2o_nif_handler_http_s {
    h2o_nif_port_listen_t super;
    h2o_req_t *req;
    struct timeval start;
    h2o_nif_handler_t *handler;
} h2o_nif_handler_http_t;

extern h2o_nif_handler_ctx_t *h2o_nif_handler_register(ErlNifEnv *env, h2o_pathconf_t *pathconf, ERL_NIF_TERM tag,
                                                       H2O_NIF_HANDLER type);
// extern void h2o_nif_handler_dtor(h2o_nif_handler_t *handler);
// extern void h2o_nif_handler_on_close(ErlNifEnv *env, h2o_nif_port_t *port);
// extern ERL_NIF_TERM h2o_nif_handler_accept(ErlNifEnv *env, h2o_nif_handler_t *handler, ERL_NIF_TERM timeout);

static int h2o_nif_handler_keep(h2o_nif_handler_t *handler);
static int h2o_nif_handler_release(h2o_nif_handler_t *handler);

inline int
h2o_nif_handler_keep(h2o_nif_handler_t *handler)
{
    return h2o_nif_resource_keep((void *)handler);
}

inline int
h2o_nif_handler_release(h2o_nif_handler_t *handler)
{
    return h2o_nif_resource_release((void *)handler);
}

#endif
