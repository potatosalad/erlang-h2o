// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../handler.h"

static int handler_event_stream_body_3_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static int handler_event_stream_body_3_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static void handler_event_stream_body_3_work(h2o_nif_ipc_message_t *super);

static h2o_nif_batch_fun_t handler_event_stream_body_3 = {handler_event_stream_body_3_exec, handler_event_stream_body_3_done};

/* handler_event_read_body/4 */

static int handler_event_read_body_4_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);

static h2o_nif_batch_fun_t handler_event_read_body_4 = {handler_event_read_body_4_exec, NULL};

static int
handler_event_read_body_4_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_read_body_4_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *handler_event = NULL;
    ErlNifPid pid;
    size_t length;
    ERL_NIF_TERM out;
    unsigned char *buf = NULL;
    if (req->argc != 3 || !h2o_nif_handler_event_get(env, req->argv[0], &handler_event) ||
        !enif_get_local_pid(env, req->argv[1], &pid) || !enif_get_ulong(env, req->argv[2], &length)) {
        return 0;
    }
    size_t expected;
    size_t desired;
    do {
        expected = atomic_load_explicit(&handler_event->entity_offset, memory_order_relaxed);
        desired = expected + length;
        if (desired > handler_event->req->entity.len) {
            length -= (desired - handler_event->req->entity.len);
            desired = handler_event->req->entity.len;
        }
        if (length == 0) {
            buf = enif_make_new_binary(env, 0, &out);
            out = enif_make_tuple3(env, ATOM_entity, handler_event->req->entity.len, out);
            (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
            return enif_send(env, &pid, NULL, out);
        }
        if (atomic_compare_exchange_weak_explicit(&handler_event->entity_offset, &expected, desired, memory_order_relaxed,
                                                  memory_order_relaxed)) {
            buf = enif_make_new_binary(env, length, &out);
            (void)memcpy(buf, handler_event->req->entity.base + expected, length);
            if (desired == handler_event->req->entity.len) {
                out = enif_make_tuple3(env, ATOM_entity, enif_make_ulong(env, handler_event->req->entity.len), out);
            } else {
                out = enif_make_tuple2(env, ATOM_entity, out);
            }
            (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
            return enif_send(env, &pid, NULL, out);
        }
        (void)ck_pr_stall();
    } while (1);
}

/* handler_event_reply/4 */

static int handler_event_reply_4_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static int handler_event_reply_4_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static void handler_event_reply_4_work(h2o_nif_ipc_message_t *super);

static h2o_nif_batch_fun_t handler_event_reply_4 = {handler_event_reply_4_exec, handler_event_reply_4_done};

static int
handler_event_reply_4_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_reply_4_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *handler_event = NULL;
    unsigned int status;
    if (req->argc != 4 || !h2o_nif_handler_event_get(env, req->argv[0], &handler_event)) {
        return 0;
    }
    assert(enif_get_uint(env, req->argv[1], &status));
    assert(enif_is_map(env, req->argv[2]));
    int previous_state = h2o_nif_port_set_finalized(&handler_event->super);
    if (previous_state == H2O_NIF_PORT_STATE_CLOSED) {
        return 0;
    }
    if ((previous_state & H2O_NIF_PORT_STATE_SEND_DATA) == H2O_NIF_PORT_STATE_SEND_DATA) {
        // convert to handler_event_stream_body/3 cast
        req->fun = &handler_event_stream_body_3;
        req->argc = 3;
        req->argv[1] = ATOM_fin;
        req->argv[2] = req->argv[3];
        return req->fun->exec(batch, env, req);
    }
    (void)atomic_fetch_add_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    return h2o_nif_ipc_batch_handler_event(handler_event, batch, req, handler_event_reply_4_work);
}

static int
handler_event_reply_4_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_reply_4_done:%s:%d\n", __FILE__, __LINE__);
    if (atomic_load_explicit(&req->refc, memory_order_relaxed) == 0) {
        return 1;
    }
    return 0;
}

static void
handler_event_reply_4_work(h2o_nif_ipc_message_t *super)
{
    TRACE_F("handler_event_reply_4_work:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_ipc_batch_req_t *message = (h2o_nif_ipc_batch_req_t *)super;
    h2o_nif_handler_event_t *handler_event = message->data.handler_event;
    h2o_nif_batch_req_t *batch_req = message->req;
    ErlNifEnv *env = message->batch->env;
    h2o_req_t *req = handler_event->req;
    unsigned int status;
    ERL_NIF_TERM headers = batch_req->argv[2];
    ErlNifBinary body;
    assert(batch_req->argc == 4);
    assert(enif_get_uint(env, batch_req->argv[1], &status));
    assert(enif_is_map(env, headers));
    assert(enif_inspect_iolist_as_binary(env, batch_req->argv[3], &body));
    req->res.status = status;
    {
        ERL_NIF_TERM key;
        ERL_NIF_TERM value;
        ErlNifMapIterator iter;
        ErlNifBinary name_bin;
        ErlNifBinary value_bin;
        assert(enif_map_iterator_create(env, headers, &iter, ERL_NIF_MAP_ITERATOR_FIRST));
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (enif_inspect_iolist_as_binary(env, key, &name_bin) && enif_inspect_iolist_as_binary(env, value, &value_bin)) {
                (void)h2o_add_header_by_str(&req->pool, &req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL,
                                            (const char *)value_bin.data, value_bin.size);
            }
            (void)enif_map_iterator_next(env, &iter);
        }
        (void)enif_map_iterator_destroy(env, &iter);
    }
    (void)h2o_send_inline(req, (const char *)body.data, body.size);
    (void)h2o_nif_port_close_silent(&handler_event->super, NULL, NULL);
    (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    (void)atomic_fetch_sub_explicit(&batch_req->refc, 1, memory_order_relaxed);
}

/* handler_event_stream_reply/3 */

static int handler_event_stream_reply_3_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static int handler_event_stream_reply_3_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req);
static void handler_event_stream_reply_3_work(h2o_nif_ipc_message_t *super);

static h2o_nif_batch_fun_t handler_event_stream_reply_3 = {handler_event_stream_reply_3_exec, handler_event_stream_reply_3_done};

static int
handler_event_stream_reply_3_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_stream_reply_3_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *handler_event = NULL;
    unsigned int status;
    if (req->argc != 3 || !h2o_nif_handler_event_get(env, req->argv[0], &handler_event)) {
        return 0;
    }
    assert(enif_get_uint(env, req->argv[1], &status));
    assert(enif_is_map(env, req->argv[2]));
    if (!h2o_nif_port_set_send_data(&handler_event->super)) {
        return 0;
    }
    (void)atomic_fetch_add_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    if (!h2o_nif_port_is_send_data(&handler_event->super)) {
        (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
        return 0;
    }
    return h2o_nif_ipc_batch_handler_event(handler_event, batch, req, handler_event_stream_reply_3_work);
}

static int
handler_event_stream_reply_3_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_stream_reply_3_done:%s:%d\n", __FILE__, __LINE__);
    if (atomic_load_explicit(&req->refc, memory_order_relaxed) == 0) {
        return 1;
    }
    return 0;
}

static void
handler_event_stream_reply_3_work(h2o_nif_ipc_message_t *super)
{
    TRACE_F("handler_event_stream_reply_3_work:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_ipc_batch_req_t *message = (h2o_nif_ipc_batch_req_t *)super;
    h2o_nif_handler_event_t *handler_event = message->data.handler_event;
    h2o_nif_batch_req_t *batch_req = message->req;
    ErlNifEnv *env = message->batch->env;
    h2o_req_t *req = handler_event->req;
    unsigned int status;
    assert(enif_get_uint(env, batch_req->argv[1], &status));
    ERL_NIF_TERM headers = batch_req->argv[2];
    req->res.status = status;
    {
        ERL_NIF_TERM key;
        ERL_NIF_TERM value;
        ErlNifMapIterator iter;
        ErlNifBinary name_bin;
        ErlNifBinary value_bin;
        assert(enif_map_iterator_create(env, headers, &iter, ERL_NIF_MAP_ITERATOR_FIRST));
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (enif_inspect_iolist_as_binary(env, key, &name_bin) && enif_inspect_iolist_as_binary(env, value, &value_bin)) {
                (void)h2o_add_header_by_str(&req->pool, &req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL,
                                            (const char *)value_bin.data, value_bin.size);
            }
            (void)enif_map_iterator_next(env, &iter);
        }
        (void)enif_map_iterator_destroy(env, &iter);
    }
    static h2o_generator_t generator = {NULL, NULL};
    (void)h2o_start_response(req, &generator);
    (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    (void)atomic_fetch_sub_explicit(&batch_req->refc, 1, memory_order_relaxed);
}

/* handler_event_stream_body/3 */

static int
handler_event_stream_body_3_exec(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_stream_body_3_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *handler_event = NULL;
    if (req->argc != 3 || !h2o_nif_handler_event_get(env, req->argv[0], &handler_event)) {
        return 0;
    }
    assert((req->argv[1] == ATOM_fin || req->argv[1] == ATOM_nofin));
    if (!h2o_nif_port_is_send_data(&handler_event->super)) {
        return 0;
    }
    if (req->argv[1] == ATOM_fin && !h2o_nif_port_set_finalized(&handler_event->super)) {
        return 0;
    }
    (void)atomic_fetch_add_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    if (req->argv[1] == ATOM_nofin && !h2o_nif_port_is_send_data(&handler_event->super)) {
        (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
        return 0;
    }
    return h2o_nif_ipc_batch_handler_event(handler_event, batch, req, handler_event_stream_body_3_work);
}

static int
handler_event_stream_body_3_done(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *req)
{
    TRACE_F("handler_event_stream_body_3_done:%s:%d\n", __FILE__, __LINE__);
    if (atomic_load_explicit(&req->refc, memory_order_relaxed) == 0) {
        return 1;
    }
    return 0;
}

static void
handler_event_stream_body_3_work(h2o_nif_ipc_message_t *super)
{
    TRACE_F("handler_event_stream_body_3_work:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_ipc_batch_req_t *message = (h2o_nif_ipc_batch_req_t *)super;
    h2o_nif_handler_event_t *handler_event = message->data.handler_event;
    h2o_nif_batch_req_t *batch_req = message->req;
    ErlNifEnv *env = message->batch->env;
    h2o_req_t *req = handler_event->req;
    ErlNifBinary body;
    assert(batch_req->argc == 3);
    assert(enif_inspect_iolist_as_binary(env, batch_req->argv[2], &body));
    h2o_iovec_t buf = h2o_strdup(&req->pool, (const char *)body.data, body.size);
    h2o_iovec_t *bufp = (void *)h2o_mem_alloc_pool(&req->pool, sizeof(*bufp));
    (void)memcpy(bufp, &buf, sizeof(*bufp));
    TRACE_F("buf=\"%.*s\"\n", bufp->len, bufp->base);
    if (batch_req->argv[1] == ATOM_fin) {
        (void)h2o_send(req, bufp, 1, H2O_SEND_STATE_FINAL);
        (void)h2o_nif_port_close_silent(&handler_event->super, NULL, NULL);
    } else {
        (void)h2o_send(req, bufp, 1, H2O_SEND_STATE_IN_PROGRESS);
    }
    (void)atomic_fetch_sub_explicit(&handler_event->num_async, 1, memory_order_relaxed);
    (void)atomic_fetch_sub_explicit(&batch_req->refc, 1, memory_order_relaxed);
}
