// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "handler.h"
#include "server.h"
#include "request.h"

// #define H2O_NIF_CONFIG(p)	(h2o_nif_config_t *)((p)->global)
// #define H2O_DRV_SERVER(c)	(h2o_drv_server_t *)((void *)(c) - sizeof (h2o_drv_port_t))

static void on_context_init(h2o_handler_t *_handler, h2o_context_t *ctx);
static int on_req(h2o_handler_t *_handler, h2o_req_t *req);

h2o_nif_handler_t *
h2o_nif_handler_alloc(ErlNifEnv *env)
{
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *handler_type = priv_data->handler;
    h2o_nif_handler_t *handler = enif_alloc_resource(handler_type, sizeof(*handler));
    if (handler == NULL) {
        return NULL;
    }
    (void)memset(handler, 0, sizeof(*handler));
    handler->priv_data = priv_data;
    return handler;
}

h2o_nif_hdl_child_t *
h2o_nif_handler_register(ErlNifEnv *env, h2o_pathconf_t *pathconf, ERL_NIF_TERM tag)
{
    TRACE_F("h2o_nif_handler_register:%s:%d\n", __FILE__, __LINE__);
    if (pathconf == NULL) {
        return NULL;
    }
    h2o_nif_config_t *config = (h2o_nif_config_t *)pathconf->global;
    h2o_nif_server_t *server = (h2o_nif_server_t *)config;
    h2o_nif_handler_t *handler = h2o_nif_handler_alloc(env);
    if (handler == NULL) {
        return NULL;
    }
    h2o_nif_hdl_child_t *child = (h2o_nif_hdl_child_t *)h2o_create_handler(pathconf, sizeof(*child));
    if (child == NULL) {
        (void)enif_release_resource((void *)handler);
        return NULL;
    }

    // TRACE_F("(0) HERE\n");

    handler->server = server;
    handler->child = child;
    handler->mtx = enif_mutex_create("h2o_handler_mutex");
    handler->env = enif_alloc_env();
    handler->tag = enif_make_copy(handler->env, tag);
    handler->refs = NULL;
    handler->num_refs = 0;
    (void)enif_self(env, &handler->pid);

    child->super.on_context_init = on_context_init;
    // child->super.on_context_dispose = on_context_dispose;
    // child->super.dispose = on_handler_dispose;
    child->super.on_req = on_req;
    child->parent = handler;

    // TRACE_F("(1) HERE\n");

    // ERL_NIF_TERM msg = enif_make_tuple3(env, enif_make_atom(env, "h2o_handler"), tag, enif_make_resource(env, (void *)handler));
    // TRACE_F("(2) HERE\n");
    // ErlNifPid to_pid;
    // (void)enif_self(env, &to_pid);
    // // TRACE_F("(3) HERE\n");
    // (void)enif_send(env, &to_pid, NULL, msg);
    // // TRACE_F("(4) HERE\n");
    // (void)enif_release_resource((void *)handler);

    // (void)enif_send(env, &to_pid, NULL, msg);
    // (void)enif_release_resource((void *)handler);

    return child;
}

void
h2o_nif_handler_dtor(ErlNifEnv *env, void *obj)
{
    return;
}

static void
on_context_init(h2o_handler_t *_handler, h2o_context_t *ctx)
{
    TRACE_F("on_context_init:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_hdl_child_t *child = (h2o_nif_hdl_child_t *)_handler;
    h2o_nif_handler_t *handler = child->parent;

    (void)enif_mutex_lock(handler->mtx);
    ERL_NIF_TERM ref;
    if (handler->refs == NULL) {
        handler->refs = enif_alloc(sizeof(handler->refs[0]));
        handler->num_refs = 1;
        ref = handler->refs[0] = enif_make_ref(handler->env);
    } else {
        handler->refs = enif_realloc(handler->refs, sizeof(handler->refs[0]) * (handler->num_refs + 1));
        handler->num_refs++;
        ref = handler->refs[handler->num_refs - 1] = enif_make_ref(handler->env);
    }
    ErlNifEnv *msg_env = enif_alloc_env();
    ERL_NIF_TERM msg =
        enif_make_tuple3(msg_env, enif_make_atom(msg_env, "h2o_handler"), enif_make_copy(msg_env, handler->tag),
                         enif_make_tuple2(msg_env, enif_make_copy(msg_env, ref), enif_make_resource(msg_env, (void *)handler)));
    if (!enif_send(NULL, &handler->pid, msg_env, msg)) {
        TRACE_F("freeing env\n");
        (void)enif_free_env(msg_env);
    }
    TRACE_F("on_context_init handler=%p\n", handler);
    (void)enif_mutex_unlock(handler->mtx);
}

static int
on_req(h2o_handler_t *_handler, h2o_req_t *req)
{
    // h2o_drv_handler_t *handler = (h2o_drv_handler_t *)_handler;
    TRACE_F("on_req:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_hdl_child_t *child = (h2o_nif_hdl_child_t *)_handler;
    h2o_nif_handler_t *handler = child->parent;
    TRACE_F("on_req          handler=%p\n", handler);
    (void)h2o_nif_request_register(handler, req);
    // static h2o_generator_t generator = {NULL, NULL};
    // req->res.status = 200;
    // req->res.reason = "OK";
    // h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain; charset=utf-8"));
    // h2o_start_response(req, &generator);
    // h2o_send(req, &req->entity, 1, 1);
    // h2o_dispose_request(req);
    // h2o_drv_request_t *request = handler->data;
    // (void) h2o_drv_request_call(request, req);
    // (void) h2o_drv_request_create(handler, req);
    return 0;
}
