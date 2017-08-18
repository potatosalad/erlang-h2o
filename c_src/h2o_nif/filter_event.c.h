// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../filter.h"
#include "../filter_event.h"
#include "../ipc.h"
#include "../slice.h"

/* fun h2o_nif:filter_event_read_start/1 */

static ERL_NIF_TERM
h2o_nif_filter_event_read_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_filter_event_read_start_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&filter_event->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!atomic_flag_test_and_set_explicit(&filter_event->rx.flag, memory_order_relaxed)) {
        (void)atomic_flag_clear_explicit(&filter_event->rx.flag, memory_order_relaxed);
        return ATOM_ok;
    } else {
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter_event->super), ATOM_ready_input);
        (void)h2o_nif_port_send(env, &filter_event->super, NULL, msg);
        return ATOM_ok;
    }
}

/* fun h2o_nif:filter_event_read/1 */

static ERL_NIF_TERM h2o_nif_filter_event_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list,
                                                       h2o_linklist_t *node);

static ERL_NIF_TERM
h2o_nif_filter_event_read_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_filter_event_read_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&filter_event->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    h2o_linklist_t queue;
    size_t num_queue;
    (void)h2o_linklist_init_anchor(&queue);
    (void)atomic_flag_clear_explicit(&filter_event->rx.flag, memory_order_relaxed);
    num_queue = atomic_load_explicit(&filter_event->rx.size, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&filter_event->rx.lock);
    (void)h2o_linklist_insert_list(&queue, &filter_event->rx.queue);
    (void)ck_spinlock_unlock(&filter_event->rx.lock);
    if (h2o_linklist_is_empty(&queue)) {
        TRACE_F("h2o_nif_filter_event_read_1:%s:%d RETURN EMPTY LIST\n", __FILE__, __LINE__);
        return enif_make_list(env, 0);
    }

    if (num_queue <= MAX_PER_SLICE) {
        ERL_NIF_TERM list;
        list = enif_make_list(env, 0);
        h2o_linklist_t *anchor = &queue;
        h2o_linklist_t *node = anchor->prev;
        h2o_nif_filter_event_ientry_t *fi = NULL;
        size_t count = 0;
        while (node != anchor) {
            fi = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_ientry_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_filter_event_ientry_make(env, fi), list);
            node = node->prev;
            count++;
        }
        while (!h2o_linklist_is_empty(&queue)) {
            fi = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_ientry_t, _link, queue.next);
            (void)h2o_linklist_unlink(&fi->_link);
            (void)mem_free(fi);
        }
        (void)atomic_fetch_sub_explicit(&filter_event->rx.size, count, memory_order_relaxed);
        TRACE_F("h2o_nif_filter_event_read_1:%s:%d RETURN LIST WITH %lu ITEMS\n", __FILE__, __LINE__, count);
        return list;
    }

    h2o_nif_slicelist_t *slicelist = NULL;
    if (!h2o_nif_slicelist_create("filter_event_read", h2o_nif_filter_event_read_1_reduce, &slicelist)) {
        return enif_make_badarg(env);
    }
    slicelist->length = num_queue;
    (void)h2o_linklist_insert_list(&slicelist->list, &queue);
    (void)h2o_nif_port_keep(&filter_event->super);
    slicelist->data = (void *)filter_event;

    return h2o_nif_slicelist_schedule(env, slicelist);
}

static ERL_NIF_TERM
h2o_nif_filter_event_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list, h2o_linklist_t *node)
{
    TRACE_F("h2o_nif_filter_event_read_1_reduce:%s:%d\n", __FILE__, __LINE__);
    /* final reduction */
    if (node == NULL) {
        h2o_nif_filter_event_t *filter_event = (void *)slicelist->data;
        h2o_nif_filter_event_ientry_t *fi = NULL;
        while (!h2o_linklist_is_empty(&slicelist->list)) {
            fi = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_ientry_t, _link, slicelist->list.next);
            (void)h2o_linklist_unlink(&fi->_link);
            (void)mem_free(fi);
        }
        (void)atomic_fetch_sub_explicit(&filter_event->rx.size, slicelist->count, memory_order_relaxed);
        (void)h2o_nif_port_release(&filter_event->super);
        return list;
    }

    /* intermediate reduction */
    h2o_nif_filter_event_ientry_t *fi = NULL;
    fi = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_ientry_t, _link, node);
    list = enif_make_list_cell(env, h2o_nif_filter_event_ientry_make(env, fi), list);
    return list;
}

// /* fun h2o_nif:filter_event_send/3 */

// typedef struct h2o_nif_filter_event_send_3_message_s h2o_nif_filter_event_send_3_message_t;

// struct h2o_nif_filter_event_send_3_message_s {
//     h2o_nif_ipc_message_t super;
//     h2o_nif_filter_event_t *event;
// };

// static void h2o_nif_filter_event_send_3_work(h2o_nif_filter_event_send_3_message_t *message);
// static void h2o_nif_filter_event_send_3_dtor(h2o_nif_filter_event_send_3_message_t *message);
// static void h2o_nif_filter_event_send_3_time(h2o_timeout_entry_t *entry);
// // static void h2o_nif_filter_event_send_3_post(h2o_timeout_entry_t *entry);

// static ERL_NIF_TERM
// h2o_nif_filter_event_send_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     TRACE_F("h2o_nif_filter_event_send_3:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *event = NULL;
//     h2o_send_state_t output_state;
//     ErlNifBinary binary;
//     if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &event) || (argv[1] != ATOM_false && argv[1] != ATOM_true) ||
//         !enif_inspect_iolist_as_binary(env, argv[2], &binary)) {
//         return enif_make_badarg(env);
//     }
//     if (h2o_nif_port_is_closed(&event->super)) {
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }
//     output_state = (argv[1] == ATOM_false) ? H2O_SEND_STATE_IN_PROGRESS : H2O_SEND_STATE_FINAL;
//     h2o_send_state_t expected = H2O_SEND_STATE_IN_PROGRESS;
//     if (output_state == H2O_SEND_STATE_FINAL &&
//         !atomic_compare_exchange_weak_explicit(&event->state.output_state, &expected, output_state, memory_order_relaxed,
//                                                memory_order_relaxed)) {
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }
//     void *p = h2o_mem_alloc_pool(&event->req->pool, sizeof(h2o_nif_filter_output_t) + binary.size);
//     h2o_nif_filter_output_t *op = (void *)p;
//     op->_link.next = op->_link.prev = NULL;
//     op->state = output_state;
//     op->iov.base = p + sizeof(h2o_nif_filter_output_t);
//     op->iov.len = binary.size;
//     (void)memcpy(op->iov.base, binary.data, binary.size);
//     (void)ck_spinlock_lock_eb(&event->state.lock);
//     (void)h2o_linklist_insert(&event->state.output, &op->_link);
//     (void)atomic_fetch_add_explicit(&event->state.num_output, 1, memory_order_relaxed);
//     (void)ck_spinlock_unlock(&event->state.lock);
//     if (!atomic_flag_test_and_set_explicit(&event->state.ready_output, memory_order_relaxed)) {
//         h2o_nif_filter_event_send_3_message_t *message =
//             (void *)h2o_nif_ipc_create_message(sizeof(*message), (h2o_nif_ipc_callback_t *)h2o_nif_filter_event_send_3_work,
//                                                (h2o_nif_ipc_callback_t *)h2o_nif_filter_event_send_3_dtor);
//         h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)event->req->conn->ctx;
//         (void)h2o_nif_port_keep(&event->super);
//         message->event = event;
//         (void)h2o_nif_ipc_enqueue(ctx->thread->ipc_queue, &message->super);
//     }
//     return ATOM_ok;
// }

// static void
// h2o_nif_filter_event_send_3_work(h2o_nif_filter_event_send_3_message_t *message)
// {
//     TRACE_F("h2o_nif_filter_event_send_3_work:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *event = message->event;
//     h2o_req_t *req = event->req;
//     if (!h2o_timeout_is_linked(&event->state.output_timeout)) {
//         event->state.output_timeout.registered_at = 0;
//         event->state.output_timeout.cb = h2o_nif_filter_event_send_3_time;
//         (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &event->state.output_timeout);
//     }
// }

// static void
// h2o_nif_filter_event_send_3_dtor(h2o_nif_filter_event_send_3_message_t *message)
// {
//     h2o_nif_filter_event_t *event = message->event;
//     (void)h2o_nif_port_release(&event->super);
// }

// static void
// h2o_nif_filter_event_send_3_time(h2o_timeout_entry_t *entry)
// {
//     TRACE_F("h2o_nif_filter_event_send_3_time:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, state.output_timeout, entry);
//     h2o_nif_filter_ostream_t *ostream = (void *)atomic_load_explicit(&event->ostream, memory_order_relaxed);
//     h2o_req_t *req = event->req;

//     h2o_linklist_t output;
//     size_t num_output;
//     h2o_linklist_t *anchor = NULL;
//     h2o_linklist_t *node = NULL;
//     h2o_nif_filter_output_t *op = NULL;
//     h2o_send_state_t send_state = H2O_SEND_STATE_IN_PROGRESS;
//     h2o_iovec_t *outbufs = NULL;
//     size_t outbufcnt;
//     size_t i;

//     (void)h2o_linklist_init_anchor(&output);
//     (void)ck_spinlock_lock_eb(&event->state.lock);
//     num_output = atomic_load_explicit(&event->state.num_output, memory_order_relaxed);
//     (void)h2o_linklist_insert_list(&output, &event->state.output);
//     assert(num_output == atomic_fetch_sub_explicit(&event->state.num_output, num_output, memory_order_relaxed));
//     (void)ck_spinlock_unlock(&event->state.lock);
//     (void)atomic_flag_clear_explicit(&event->state.ready_output, memory_order_relaxed);

//     anchor = &output;
//     node = anchor->next;
//     outbufcnt = 0;
//     while (node != anchor) {
//         node = node->next;
//         outbufcnt++;
//     }
//     assert(outbufcnt == num_output);

//     outbufs = h2o_mem_alloc_pool(&req->pool, sizeof(h2o_iovec_t) * outbufcnt);
//     node = anchor->next;
//     for (i = 0; i < outbufcnt; ++i) {
//         op = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_output_t, _link, node);
//         node = node->next;
//         if (op->state == H2O_SEND_STATE_FINAL) {
//             send_state = op->state;
//         }
//         outbufs[i] = op->iov;
//         // TRACE_F("sending: \"%.*s\" (%s)\n", op->iov.len, op->iov.base, (send_state == H2O_SEND_STATE_FINAL) ? "final" :
//         // "in_progress");
//     }
//     (void)h2o_ostream_send_next(&ostream->super, req, outbufs, outbufcnt, send_state);

//     if (h2o_timeout_is_linked(&event->state.output_timeout)) {
//         (void)h2o_timeout_unlink(&event->state.output_timeout);
//     }

//     if (send_state == H2O_SEND_STATE_FINAL) {
//         // event->state.output_timeout.registered_at = 0;
//         // event->state.output_timeout.cb = h2o_nif_filter_event_send_3_post;
//         // (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->one_sec_timeout, &event->state.output_timeout);
//         (void)h2o_nif_port_close_silent(&event->super, NULL, NULL);
//     }
// }

// static void
// h2o_nif_filter_event_send_3_post(h2o_timeout_entry_t *entry)
// {
//     TRACE_F("h2o_nif_filter_event_send_3_post:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, state.output_timeout, entry);
//     if (h2o_timeout_is_linked(&event->state.output_timeout)) {
//         (void)h2o_timeout_unlink(&event->state.output_timeout);
//     }
//     (void)h2o_nif_port_close_silent(&event->super, NULL, NULL);
// }
