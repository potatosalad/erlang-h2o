// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "request.h"
#include "server.h"
#include "handler.h"

h2o_nif_request_t *
h2o_nif_request_alloc(h2o_nif_handler_t *handler)
{
    // h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    // TRACE_F("priv_data = %p\n", priv_data);
    ErlNifResourceType *request_type = handler->priv_data->request;
    h2o_nif_request_t *request = enif_alloc_resource(request_type, sizeof(*request));
    if (request == NULL) {
        return NULL;
    }
    (void)memset(request, 0, sizeof(*request));
    return request;
}

h2o_nif_request_t *
h2o_nif_request_register(h2o_nif_handler_t *handler, h2o_req_t *req)
{
    TRACE_F("h2o_nif_request_register:%s:%d\n", __FILE__, __LINE__);
    if (handler == NULL || req == NULL) {
        return NULL;
    }
    h2o_nif_request_t *request = h2o_nif_request_alloc(handler);
    if (request == NULL) {
        return NULL;
    }
    request->handler = handler;
    request->req = req;
    TRACE_F("sending term\n");
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM msg = enif_make_tuple3(msg_env, enif_make_atom(msg_env, "h2o_request"), enif_make_copy(msg_env, handler->tag),
                                        enif_make_resource(msg_env, (void *)request));
    if (!enif_send(NULL, &handler->pid, msg_env, msg)) {
        TRACE_F("freeing request env\n");
        (void)enif_free_env(msg_env);
    }
    return request;
}

void
h2o_nif_request_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_request_dtor:%s:%d\n", __FILE__, __LINE__);
    return;
}
