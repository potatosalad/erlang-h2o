// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../filter_event.h"

// /* filter_event_read/1 */

// static int filter_event_read_1_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static int filter_event_read_1_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// static h2o_nif_batch_fun_t filter_event_read_1 = {.test = filter_event_read_1_test, .exec = filter_event_read_1_exec, .done = NULL};

// static int
// filter_event_read_1_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     TRACE_F("filter_event_read_1_test:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *filter_event = NULL;
//     if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
//         return 0;
//     }
//     return 1;
// }

// static int
// filter_event_read_1_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     TRACE_F("filter_event_read_1_exec:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *filter_event = NULL;
//     ERL_NIF_TERM out;
//     if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &filter_event)) {
//         out = enif_make_tuple2(env, ATOM_error, ATOM_closed);
//         out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
//         (void)enif_send(env, &ctx->req->from.to, NULL, out);
//         return 0;
//     }
//     if (h2o_nif_port_is_closed(&filter_event->super)) {
//         out = enif_make_tuple2(env, ATOM_error, ATOM_closed);
//         out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
//         (void)enif_send(env, &ctx->req->from.to, NULL, out);
//         return 1;
//     }

//     h2o_linklist_t input;
//     size_t num_input;
//     (void)atomic_flag_clear_explicit(&filter_event->state.ready_input, memory_order_relaxed);
//     (void)h2o_linklist_init_anchor(&input);
//     num_input = atomic_load_explicit(&filter_event->state.num_input, memory_order_relaxed);
//     (void)ck_spinlock_lock_eb(&filter_event->state.lock);
//     (void)h2o_linklist_insert_list(&input, &filter_event->state.input);
//     (void)ck_spinlock_unlock(&filter_event->state.lock);

//     if (h2o_linklist_is_empty(&input)) {
//         out = enif_make_list(env, 0);
//         out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), out);
//         (void)enif_send(env, &ctx->req->from.to, NULL, out);
//         return 1;
//     }

//     ERL_NIF_TERM list;
//     ERL_NIF_TERM binary;
//     list = enif_make_list(env, 0);
//     h2o_linklist_t *anchor = &input;
//     h2o_linklist_t *node = anchor->prev;
//     h2o_nif_filter_input_t *ip = NULL;
//     unsigned long count = 0;
//     while (node != anchor) {
//         ip = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_input_t, _link, node);
//         binary = enif_make_binary(env, &ip->binary);
//         binary =
//             enif_make_sub_binary(env, binary, sizeof(h2o_nif_filter_input_t), ip->binary.size - sizeof(h2o_nif_filter_input_t));
//         list = enif_make_list_cell(env, binary, list);
//         node = node->prev;
//         count++;
//     }
//     (void)atomic_fetch_sub_explicit(&filter_event->state.num_input, count, memory_order_relaxed);

//     out = enif_make_tuple2(env, enif_make_copy(env, ctx->req->from.tag), list);
//     (void)enif_send(env, &ctx->req->from.to, NULL, out);
//     return 1;
// }

// /* cast_filter_event_read_entity/3 */

// static int cast_filter_event_read_entity_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
// static int cast_filter_event_read_entity_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// static h2o_nif_batch_fun_t cast_filter_event_read_entity_3 = {
//     .test = cast_filter_event_read_entity_3_test, .exec = cast_filter_event_read_entity_3_exec, .done = NULL};

// static int
// cast_filter_event_read_entity_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     TRACE_F("filter_event_read_entity_3_test:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *filter_event = NULL;
//     ErlNifPid pid;
//     size_t length;
//     if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || !enif_get_local_pid(env, argv[1], &pid) ||
//         !enif_get_ulong(env, argv[2], &length)) {
//         return 0;
//     }
//     return 1;
// }

// static int
// cast_filter_event_read_entity_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     TRACE_F("filter_event_read_entity_3_exec:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_filter_event_t *filter_event = NULL;
//     ErlNifPid pid;
//     size_t length;
//     ERL_NIF_TERM out;
//     unsigned char *buf = NULL;
//     if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || !enif_get_local_pid(env, argv[1], &pid) ||
//         !enif_get_ulong(env, argv[2], &length)) {
//         return 0;
//     }
//     size_t expected;
//     size_t desired;
//     do {
//         expected = atomic_load_explicit(&filter_event->entity_offset, memory_order_relaxed);
//         desired = expected + length;
//         if (desired > filter_event->req->entity.len) {
//             length -= (desired - filter_event->req->entity.len);
//             desired = filter_event->req->entity.len;
//         }
//         if (length == 0) {
//             buf = enif_make_new_binary(env, 0, &out);
//             out = enif_make_tuple3(env, ATOM_entity, filter_event->req->entity.len, out);
//             return enif_send(env, &pid, NULL, out);
//         }
//         if (atomic_compare_exchange_weak_explicit(&filter_event->entity_offset, &expected, desired, memory_order_relaxed,
//                                                   memory_order_relaxed)) {
//             buf = enif_make_new_binary(env, length, &out);
//             (void)memcpy(buf, filter_event->req->entity.base + expected, length);
//             if (desired == filter_event->req->entity.len) {
//                 out = enif_make_tuple3(env, ATOM_entity, enif_make_ulong(env, filter_event->req->entity.len), out);
//             } else {
//                 out = enif_make_tuple2(env, ATOM_entity, out);
//             }
//             return enif_send(env, &pid, NULL, out);
//         }
//         (void)ck_pr_stall();
//     } while (1);
// }

/* cast: filter_event_send/3 */

static int filter_event_send_3_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int filter_event_send_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

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
    return 1;
}

static int
filter_event_send_3_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("filter_event_send_3_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = NULL;
    h2o_send_state_t tx_state;
    ErlNifBinary binary;
    h2o_iovec_t buf;
    if (argc != 3 || !h2o_nif_filter_event_get(env, argv[0], &filter_event) || (argv[1] != ATOM_fin && argv[1] != ATOM_nofin) ||
        !enif_inspect_iolist_as_binary(env, argv[2], &binary)) {
        return 0;
    }
    tx_state = (argv[1] == ATOM_nofin) ? H2O_SEND_STATE_IN_PROGRESS : H2O_SEND_STATE_FINAL;
    buf = h2o_iovec_init(binary.data, binary.size);
    if (!h2o_nif_filter_event_send(filter_event, &buf, 1, tx_state)) {
        return 0;
    }
    return 1;
}
