// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_REQUEST_H
#define H2O_NIF_REQUEST_H

#include "globals.h"

struct h2o_nif_request_s {
    h2o_linklist_t _link;
    h2o_nif_handler_t *handler;
    h2o_req_t *req;
    h2o_websocket_conn_t *ws;
    ErlNifPid *wspid;
    ErlNifPid _wspid;
    // h2o_nif_queue_t wsacc;
    // h2o_nif_queue_t wsmsg;
};

extern h2o_nif_request_t *h2o_nif_request_alloc(h2o_nif_handler_t *handler);
extern h2o_nif_request_t *h2o_nif_request_create(h2o_nif_handler_t *handler, h2o_req_t *req);
extern int h2o_nif_request_dispatch(h2o_nif_request_t *request);
extern void h2o_nif_request_dtor(ErlNifEnv *env, void *obj);
extern void h2o_nif_request_on_ws_message(h2o_websocket_conn_t *conn, const struct wslay_event_on_msg_recv_arg *arg);

#endif