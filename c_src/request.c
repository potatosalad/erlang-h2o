// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "request.h"

static H2O_NIF_RETURN h2o_nif_request_on_close(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx);
static void h2o_nif_request_dtor(void *block);

int
h2o_nif_request_accept_http(h2o_nif_port_t *parent, h2o_nif_handler_t *handler, h2o_req_t *req, h2o_nif_port_t **portp)
{
    TRACE_F("h2o_nif_request_accept_http:%s:%d\n", __FILE__, __LINE__);

    h2o_nif_port_t *child = NULL;

    /* allocate request */
    h2o_nif_request_t *request = h2o_nif_resource_alloc(sizeof(*request));
    if (request == NULL) {
        return 0;
    }
    request->resource.dtor = h2o_nif_request_dtor;
    (void)atomic_init(&request->finalizer, 1);
    request->type = H2O_NIF_REQUEST_HTTP;
    (void)h2o_nif_handler_keep(handler);
    request->handler = handler;
    request->req = req;

    /* open child port */
    if (!h2o_nif_port_open(parent, &child)) {
        (void)h2o_nif_request_release(request);
        return 0;
    }
    (void)h2o_nif_port_keep(child);
    request->port = child;
    child->on_close.callback = h2o_nif_request_on_close;
    child->type = H2O_NIF_PORT_TYPE_REQUEST;
    child->data = (void *)request;
    (void)h2o_nif_port_cas_set_state(child, H2O_NIF_PORT_STATE_ACCEPTING);

    *portp = child;

    return 1;
}

static H2O_NIF_RETURN
h2o_nif_request_on_close(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx)
{
    TRACE_F("h2o_nif_request_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_REQUEST);
    h2o_nif_request_t *request = (h2o_nif_request_t *)port->data;
    (void)h2o_nif_request_release(request);
    return H2O_NIF_RETURN_DONE;
}

static void
h2o_nif_request_dtor(void *block)
{
    TRACE_F("h2o_nif_request_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_request_t *request = (h2o_nif_request_t *)block;
    h2o_nif_port_t *port = request->port;
    h2o_nif_handler_t *handler = request->handler;
    if (handler != NULL) {
        (void)h2o_nif_handler_release(handler);
    }
    // Unlink handler from port (handler is now unsafe to use)
    if (port != NULL) {
        port->data = NULL;
        (void)h2o_nif_port_release(port);
    }
    return;
}
