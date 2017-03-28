// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../filter_event.h"

/* filter_event_read/1 */

static int filter_event_read_1_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int filter_event_read_1_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static h2o_nif_batch_fun_t filter_event_read_1 = {.test = filter_event_read_1_test, .exec = filter_event_read_1_exec, .done = NULL};

static int
filter_event_read_1_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_read_1_test:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
        return 0;
    }
    return 1;
}

static int
filter_event_read_1_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_read_1_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    ERL_NIF_TERM out;
    if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
        out = enif_make_tuple2(env, ATOM_error, ATOM_closed);
        out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
        (void)enif_send(env, &ctx->req->from.to, NULL, out);
        return 0;
    }
    if (h2o_nif_port_is_closed(&filter_event->super)) {
        out = enif_make_tuple2(env, ATOM_error, ATOM_closed);
        out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
        (void)enif_send(env, &ctx->req->from.to, NULL, out);
        return 1;
    }

    h2o_linklist_t input;
    size_t num_input;
    (void)atomic_flag_clear_explicit(&filter_event->state.ready_input, memory_order_relaxed);
    (void)h2o_linklist_init_anchor(&input);
    num_input = atomic_load_explicit(&filter_event->state.num_input, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&filter_event->state.lock);
    (void)h2o_linklist_insert_list(&input, &filter_event->state.input);
    (void)ck_spinlock_unlock(&filter_event->state.lock);

    if (h2o_linklist_is_empty(&input)) {
        out = enif_make_list(env, 0);
        out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
        (void)enif_send(env, &ctx->req->from.to, NULL, out);
        return 1;
    }

    ERL_NIF_TERM list;
    ERL_NIF_TERM binary;
    list = enif_make_list(env, 0);
    h2o_linklist_t *anchor = &input;
    h2o_linklist_t *node = anchor->prev;
    h2o_nif_filter_input_t *ip = NULL;
    unsigned long count = 0;
    while (node != anchor) {
        ip = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_input_t, _link, node);
        binary = enif_make_binary(env, &ip->binary);
        binary =
            enif_make_sub_binary(env, binary, sizeof(h2o_nif_filter_input_t), ip->binary.size - sizeof(h2o_nif_filter_input_t));
        list = enif_make_list_cell(env, binary, list);
        node = node->prev;
        count++;
    }
    (void)atomic_fetch_sub_explicit(&filter_event->state.num_input, count, memory_order_relaxed);

    out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), list);
    (void)enif_send(env, &ctx->req->from.to, NULL, out);
    return 1;
}

/* filter_event_read_entity/3 */

static int filter_event_read_entity_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int filter_event_read_entity_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static h2o_nif_batch_fun_t filter_event_read_entity_3 = {
    .test = filter_event_read_entity_3_test, .exec = filter_event_read_entity_3_exec, .done = NULL};

static int
filter_event_read_entity_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_read_entity_3_test:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    ErlNifPid pid;
    size_t length;
    if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || !enif_get_local_pid(env, argv[1], &pid) ||
        !enif_get_ulong(env, argv[2], &length)) {
        return 0;
    }
    return 1;
}

static int
filter_event_read_entity_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_read_entity_3_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    ErlNifPid pid;
    size_t length;
    ERL_NIF_TERM out;
    unsigned char *buf = NULL;
    if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || !enif_get_local_pid(env, argv[1], &pid) ||
        !enif_get_ulong(env, argv[2], &length)) {
        return 0;
    }
    size_t expected;
    size_t desired;
    do {
        expected = atomic_load_explicit(&filter_event->entity_offset, memory_order_relaxed);
        desired = expected + length;
        if (desired > filter_event->req->entity.len) {
            length -= (desired - filter_event->req->entity.len);
            desired = filter_event->req->entity.len;
        }
        if (length == 0) {
            buf = enif_make_new_binary(env, 0, &out);
            out = enif_make_tuple3(env, ATOM_entity, filter_event->req->entity.len, out);
            return enif_send(env, &pid, NULL, out);
        }
        if (atomic_compare_exchange_weak_explicit(&filter_event->entity_offset, &expected, desired, memory_order_relaxed,
                                                  memory_order_relaxed)) {
            buf = enif_make_new_binary(env, length, &out);
            (void)memcpy(buf, filter_event->req->entity.base + expected, length);
            if (desired == filter_event->req->entity.len) {
                out = enif_make_tuple3(env, ATOM_entity, enif_make_ulong(env, filter_event->req->entity.len), out);
            } else {
                out = enif_make_tuple2(env, ATOM_entity, out);
            }
            return enif_send(env, &pid, NULL, out);
        }
        (void)ck_pr_stall();
    } while (1);
}

/* filter_event_send/3 */

typedef struct h2o_nif_filter_event_send_3_async_s h2o_nif_filter_event_send_3_async_t;

struct h2o_nif_filter_event_send_3_async_s {
    h2o_nif_ipc_message_t super;
    h2o_nif_batch_t *batch;
    h2o_nif_filter_event_t *event;
};

static int filter_event_send_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int filter_event_send_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static void h2o_nif_filter_event_send_3_async_work(h2o_nif_filter_event_send_3_async_t *message);
static void h2o_nif_filter_event_send_3_async_dtor(h2o_nif_filter_event_send_3_async_t *message);
static void h2o_nif_filter_event_send_3_async_time(h2o_timeout_entry_t *entry);
static void h2o_nif_filter_event_send_3_async_down(h2o_timeout_entry_t *entry);

static h2o_nif_batch_fun_t filter_event_send_3 = {.test = filter_event_send_3_test, .exec = filter_event_send_3_exec, .done = NULL};

static int
filter_event_send_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_send_3_test:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    ErlNifBinary binary;
    if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || (argv[1] != ATOM_fin && argv[1] != ATOM_nofin) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &binary)) {
        return 0;
    }
    if (h2o_nif_port_is_closed(&filter_event->super)) {
        return 0;
    }
    return 1;
}

static int
filter_event_send_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_send_3_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    h2o_send_state_t output_state;
    ErlNifBinary binary;
    if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || (argv[1] != ATOM_fin && argv[1] != ATOM_nofin) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &binary)) {
        return 0;
    }
    if (h2o_nif_port_is_closed(&filter_event->super)) {
        return 0;
    }
    output_state = (argv[1] == ATOM_nofin) ? H2O_SEND_STATE_IN_PROGRESS : H2O_SEND_STATE_FINAL;
    {
        h2o_send_state_t expected = H2O_SEND_STATE_IN_PROGRESS;
        if (output_state == H2O_SEND_STATE_FINAL &&
            !atomic_compare_exchange_weak_explicit(&filter_event->state.output_state, &expected, output_state, memory_order_relaxed,
                                                   memory_order_relaxed)) {
            return 0;
        }
    }
    h2o_req_t *req = filter_event->req;
    h2o_nif_filter_output_t *op = (void *)h2o_mem_alloc_pool(&req->pool, sizeof(h2o_nif_filter_output_t) + binary.size);
    op->_link.next = op->_link.prev = NULL;
    op->state = output_state;
    op->iov.base = ((void *)op) + sizeof(h2o_nif_filter_output_t);
    op->iov.len = binary.size;
    (void)memcpy(op->iov.base, binary.data, binary.size);
    (void)ck_spinlock_lock_eb(&filter_event->state.lock);
    (void)h2o_linklist_insert(&filter_event->state.output, &op->_link);
    (void)atomic_fetch_add_explicit(&filter_event->state.num_output, 1, memory_order_relaxed);
    (void)ck_spinlock_unlock(&filter_event->state.lock);
    if (!atomic_flag_test_and_set_explicit(&filter_event->state.ready_output, memory_order_relaxed)) {
        h2o_nif_filter_event_send_3_async_t *message =
            (void *)h2o_nif_ipc_create_message(sizeof(*message), (h2o_nif_ipc_callback_t *)h2o_nif_filter_event_send_3_async_work,
                                               (h2o_nif_ipc_callback_t *)h2o_nif_filter_event_send_3_async_dtor);
        h2o_nif_srv_thread_ctx_t *thread_ctx = (h2o_nif_srv_thread_ctx_t *)req->conn->ctx;
        (void)h2o_nif_batch_keep(ctx->batch);
        message->batch = ctx->batch;
        (void)h2o_nif_port_keep(&filter_event->super);
        message->event = filter_event;
        (void)h2o_nif_ipc_enqueue(thread_ctx->thread->ipc_queue, &message->super);
    }
    return 1;
}

static void
h2o_nif_filter_event_send_3_async_work(h2o_nif_filter_event_send_3_async_t *message)
{
    TRACE_F("h2o_nif_filter_event_send_3_async_work:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = message->event;
    h2o_req_t *req = filter_event->req;
    uintptr_t expected = (uintptr_t)NULL;
    uintptr_t desired = (uintptr_t)message->batch;
    if (atomic_compare_exchange_weak_explicit(&filter_event->state.output_batch, &expected, desired, memory_order_relaxed,
                                              memory_order_relaxed)) {
        assert(!h2o_timeout_is_linked(&filter_event->state.output_timeout));
        // (void)h2o_nif_batch_keep(message->batch);
        filter_event->state.output_timeout.registered_at = 0;
        filter_event->state.output_timeout.cb = h2o_nif_filter_event_send_3_async_time;
        (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &filter_event->state.output_timeout);
        return;
    }
    (void)h2o_nif_port_release(&filter_event->super);
    (void)h2o_nif_batch_release(message->batch);
}

static void
h2o_nif_filter_event_send_3_async_dtor(h2o_nif_filter_event_send_3_async_t *message)
{
    TRACE_F("h2o_nif_filter_event_send_3_async_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = message->event;
    (void)h2o_nif_port_release(&filter_event->super);
}

static void
h2o_nif_filter_event_send_3_async_time(h2o_timeout_entry_t *entry)
{
    TRACE_F("h2o_nif_filter_event_send_3_async_time:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, state.output_timeout, entry);
    if (filter_event == NULL) {
        return;
    }
    h2o_nif_batch_t *batch =
        (void *)atomic_exchange_explicit(&filter_event->state.output_batch, (uintptr_t)NULL, memory_order_relaxed);
    if (batch == NULL) {
        (void)h2o_nif_port_release(&filter_event->super);
        return;
    }
    h2o_nif_filter_ostream_t *ostream = (void *)atomic_load_explicit(&filter_event->ostream, memory_order_relaxed);
    h2o_req_t *req = filter_event->req;

    h2o_linklist_t output;
    size_t num_output;
    h2o_linklist_t *anchor = NULL;
    h2o_linklist_t *node = NULL;
    h2o_nif_filter_output_t *op = NULL;
    h2o_send_state_t send_state = H2O_SEND_STATE_IN_PROGRESS;
    h2o_iovec_t *outbufs = NULL;
    size_t outbufcnt;
    size_t i;

    (void)h2o_linklist_init_anchor(&output);
    (void)ck_spinlock_lock_eb(&filter_event->state.lock);
    num_output = atomic_load_explicit(&filter_event->state.num_output, memory_order_relaxed);
    (void)h2o_linklist_insert_list(&output, &filter_event->state.output);
    assert(num_output == atomic_fetch_sub_explicit(&filter_event->state.num_output, num_output, memory_order_relaxed));
    (void)ck_spinlock_unlock(&filter_event->state.lock);
    (void)atomic_flag_clear_explicit(&filter_event->state.ready_output, memory_order_relaxed);

    anchor = &output;
    node = anchor->next;
    outbufcnt = 0;
    while (node != anchor) {
        node = node->next;
        outbufcnt++;
    }
    assert(outbufcnt == num_output);

    outbufs = h2o_mem_alloc_pool(&req->pool, sizeof(h2o_iovec_t) * outbufcnt);
    node = anchor->next;
    for (i = 0; i < outbufcnt; ++i) {
        op = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_output_t, _link, node);
        node = node->next;
        if (op->state == H2O_SEND_STATE_FINAL) {
            send_state = op->state;
        }
        outbufs[i] = op->iov;
    }
    (void)h2o_ostream_send_next(&ostream->super, req, outbufs, outbufcnt, send_state);

    if (h2o_timeout_is_linked(&filter_event->state.output_timeout)) {
        (void)h2o_timeout_unlink(&filter_event->state.output_timeout);
    }

    if (send_state == H2O_SEND_STATE_FINAL) {
        (void)atomic_store_explicit(&filter_event->state.output_batch, (uintptr_t)batch, memory_order_relaxed);
        filter_event->state.output_timeout.registered_at = 0;
        filter_event->state.output_timeout.cb = h2o_nif_filter_event_send_3_async_down;
        (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &filter_event->state.output_timeout);
        return;
        // (void)h2o_nif_port_close_silent(&filter_event->super, NULL, NULL);
    }

    (void)h2o_nif_port_release(&filter_event->super);
    (void)h2o_nif_batch_release(batch);
}

static void
h2o_nif_filter_event_send_3_async_down(h2o_timeout_entry_t *entry)
{
    TRACE_F("h2o_nif_filter_event_send_3_async_down:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, state.output_timeout, entry);
    if (filter_event == NULL) {
        return;
    }
    h2o_nif_batch_t *batch =
        (void *)atomic_exchange_explicit(&filter_event->state.output_batch, (uintptr_t)NULL, memory_order_relaxed);
    if (batch == NULL) {
        (void)h2o_nif_port_release(&filter_event->super);
        return;
    }
    (void)h2o_nif_port_close_silent(&filter_event->super, NULL, NULL);
    (void)h2o_nif_port_release(&filter_event->super);
    (void)h2o_nif_batch_release(batch);
}
