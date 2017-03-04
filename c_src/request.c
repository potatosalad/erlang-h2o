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
h2o_nif_request_create(h2o_nif_handler_t *handler, h2o_req_t *req)
{
    TRACE_F("h2o_nif_request_create:%s:%d\n", __FILE__, __LINE__);
    if (handler == NULL || req == NULL) {
        return NULL;
    }
    h2o_nif_request_t *request = h2o_nif_request_alloc(handler);
    if (request == NULL) {
        return NULL;
    }
    // (void)h2o_linklist_init_anchor(&request->_link);
    request->_link.next = request->_link.prev = NULL;
    request->handler = handler;
    request->req = req;
    request->ws = NULL;
    request->wspid = NULL;
    return request;
}

int
h2o_nif_request_dispatch(h2o_nif_request_t *request)
{
    if (request == NULL) {
        return 0;
    }
    h2o_nif_handler_t *handler = request->handler;
    // request->wsacc.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&request->wsacc.pending);
    // request->wsmsg.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&request->wsmsg.pending);
    (void)enif_mutex_lock(handler->acc.mutex);
    if (!h2o_linklist_is_empty(&handler->acc.pending)) {
        h2o_nif_accept_t *acc = (h2o_nif_accept_t *)handler->acc.pending.next;
        int retval;
        (void)h2o_linklist_unlink(&acc->_link);
        (void)enif_mutex_unlock(handler->acc.mutex);
        // TRACE_F("acceptor found: %d\n", acc->id);
        ErlNifEnv *msg_env = enif_alloc_env();
        ERL_NIF_TERM msg = enif_make_tuple2(msg_env, enif_make_int(msg_env, acc->id), enif_make_resource(msg_env, (void *)request));
        // ERL_NIF_TERM msg = enif_make_tuple3(msg_env, enif_make_atom(msg_env, "h2o_request"), enif_make_copy(msg_env,
        // handler->tag),
        //                                     enif_make_resource(msg_env, (void *)request));
        if (request->ws != NULL) {
            request->_wspid = acc->caller;
            request->wspid = &request->_wspid;
        }
        if (!(retval = enif_send(NULL, &acc->caller, msg_env, msg))) {
            // TRACE_F("freeing request env\n");
            (void)enif_free_env(msg_env);
        }
        (void)enif_release_resource((void *)request);
        (void)enif_free(acc);
        return retval;
    }
    (void)enif_mutex_unlock(handler->acc.mutex);
    (void)enif_mutex_lock(handler->req.mutex);
    // TRACE_F("enqueueing request\n");
    (void)h2o_linklist_insert(&handler->req.pending, &request->_link);
    (void)enif_mutex_unlock(handler->req.mutex);
    // pthread_mutex_lock(&queue.mutex);

    // h2o_linklist_insert(&queue.pending, &req->_pending);

    // if (queue.num_threads_idle == 0 && queue.num_threads < h2o_hostinfo_max_threads)
    //     create_lookup_thread();

    // pthread_cond_signal(&queue.cond);
    // pthread_mutex_unlock(&queue.mutex);

    // TRACE_F("sending term\n");
    // ErlNifEnv *msg_env = enif_alloc_env();
    // ERL_NIF_TERM msg = enif_make_tuple3(msg_env, enif_make_atom(msg_env, "h2o_request"), enif_make_copy(msg_env, handler->tag),
    //                                     enif_make_resource(msg_env, (void *)request));
    // if (!enif_send(NULL, &handler->caller, msg_env, msg)) {
    //     TRACE_F("freeing request env\n");
    //     (void)enif_free_env(msg_env);
    // }
    return 1;
}

void
h2o_nif_request_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_request_dtor:%s:%d\n", __FILE__, __LINE__);
    return;
}

void
h2o_nif_request_on_ws_message(h2o_websocket_conn_t *conn, const struct wslay_event_on_msg_recv_arg *arg)
{
    h2o_nif_request_t *request = (h2o_nif_request_t *)conn->data;
    if (arg == NULL) {
        TRACE_F("closing websocket\n");
        (void)h2o_websocket_close(conn);
        // notify all acceptors
        return;
    }

    if (request->wspid == NULL) {
        TRACE_F("ignoring message\n");
    } else {
        TRACE_F("snd frame: %u\n", arg->opcode);
        ErlNifEnv *msg_env = enif_alloc_env();
        ERL_NIF_TERM ws_msg;
        unsigned char *ws_msg_buf = enif_make_new_binary(msg_env, arg->msg_length, &ws_msg);
        (void)memcpy(ws_msg_buf, arg->msg, arg->msg_length);
        ERL_NIF_TERM msg = enif_make_tuple4(msg_env, enif_make_uint(msg_env, arg->rsv), enif_make_uint(msg_env, arg->opcode),
                                            ws_msg, enif_make_uint(msg_env, arg->status_code));
        if (!enif_send(NULL, request->wspid, msg_env, msg)) {
            (void)enif_free_env(msg_env);
        }
    }

    if (!wslay_is_ctrl_frame(arg->opcode)) {
        TRACE_F("got frame: %u\n", arg->opcode);
        struct wslay_event_msg msgarg = {arg->opcode, arg->msg, arg->msg_length};
        wslay_event_queue_msg(conn->ws_ctx, &msgarg);
        return;
    }

    // h2o_nif_request_t *request = (h2o_nif_request_t *)conn->data;
    // TRACE_F("snd frame: %u\n", arg->opcode);
    // ErlNifEnv *msg_env = enif_alloc_env();
    // ERL_NIF_TERM ws_msg;
    // unsigned char *ws_msg_buf = enif_make_new_binary(msg_env, arg->msg_length, &ws_msg);
    // (void)memcpy(ws_msg_buf, arg->msg, arg->msg_length);
    // ERL_NIF_TERM msg = enif_make_tuple4(msg_env,
    //     enif_make_uint(msg_env, arg->rsv),
    //     enif_make_uint(msg_env, arg->opcode),
    //     ws_msg,
    //     enif_make_uint(msg_env, arg->status_code));
    // if (!enif_send(NULL, &request->wspid, msg_env, msg)) {
    //     (void)enif_free_env(msg_env);
    // }
}
