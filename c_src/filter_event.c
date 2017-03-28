// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "filter_event.h"
#include <h2o.h>
#include <h2o/configurator.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <h2o/http2_internal.h>
#include <h2o/serverutil.h>

/* Types */

typedef struct __deferred_action_s __deferred_action_t;

struct __deferred_action_s {
    h2o_timeout_entry_t timeout;
    h2o_nif_filter_event_t *event;
    h2o_req_t *req;
};

static void on_send(h2o_ostream_t *super, h2o_req_t *req, h2o_iovec_t *inbufs, size_t inbufcnt, h2o_send_state_t state);
static void on_stop(h2o_ostream_t *super, h2o_req_t *req);
static __deferred_action_t *create_deferred_action(h2o_nif_filter_event_t *event, h2o_req_t *req, h2o_timeout_cb cb);
static void on_deferred_action_dispose(void *_action);
static void on_ready_input_cb(h2o_timeout_entry_t *entry);
// static void on_final_input_cb(h2o_timeout_entry_t *entry);

/* Resource Functions */

ERL_NIF_TERM
h2o_nif_filter_event_make(ErlNifEnv *env, h2o_nif_filter_event_t *event)
{
    TRACE_F("h2o_nif_filter_event_make:%s:%d\n", __FILE__, __LINE__);

#define h2o_iovec_to_binary(iov, binp)                                                                                             \
    do {                                                                                                                           \
        buf = enif_make_new_binary(env, (iov).len, binp);                                                                          \
        (void)memcpy(buf, (iov).base, (iov).len);                                                                                  \
    } while (0)

    h2o_req_t *req = event->req;
    h2o_http2_stream_t *stream = NULL;
    ERL_NIF_TERM tmp;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    unsigned char *buf = NULL;
    ERL_NIF_TERM tuple[23];
    size_t i = 0;
    if (req->version >= 0x200) {
        stream = H2O_STRUCT_FROM_MEMBER(h2o_http2_stream_t, req, req);
    }
    // if (req->entity.base != NULL) {
    //     DEBUG_F("entity: %.*s\n", req->entity.len, req->entity.base);
    // }
    /* record: h2o_req */
    tuple[i++] = ATOM_h2o_req;
    /* event */
    tuple[i++] = h2o_nif_port_make(env, &event->super);
    /* event_type */
    tuple[i++] = ATOM_filter;
    /* authority */
    h2o_iovec_to_binary(req->authority, &tmp);
    tuple[i++] = tmp;
    /* body_length */
    tuple[i++] = (req->entity.base == NULL) ? enif_make_ulong(env, 0) : enif_make_ulong(env, req->entity.len);
    /* has_body */
    tuple[i++] = (req->entity.base == NULL) ? ATOM_false : ATOM_true;
    /* has_read_body */
    tuple[i++] = ATOM_false;
    /* has_sent_resp */
    tuple[i++] = ATOM_false;
    /* headers */
    {
        h2o_header_t header;
        ERL_NIF_TERM slist[req->headers.size];
        size_t j;
        for (j = 0; j < req->headers.size; j++) {
            header = req->headers.entries[j];
            buf = enif_make_new_binary(env, header.name->len, &key);
            (void)memcpy(buf, header.name->base, header.name->len);
            buf = enif_make_new_binary(env, header.value.len, &val);
            (void)memcpy(buf, header.value.base, header.value.len);
            slist[j] = enif_make_tuple2(env, key, val);
        }
        tmp = enif_make_list_from_array(env, slist, req->headers.size);
        tuple[i++] = tmp;
    }
    /* host */
    h2o_iovec_to_binary(req->hostconf->authority.host, &tmp);
    tuple[i++] = tmp;
    /* method */
    h2o_iovec_to_binary(req->method, &tmp);
    tuple[i++] = tmp;
    /* multipart */
    tuple[i++] = ATOM_undefined;
    /* path */
    h2o_iovec_to_binary(req->path, &tmp);
    tuple[i++] = tmp;
    /* peer */
    {
        ERL_NIF_TERM peer;
        struct sockaddr_storage ss;
        socklen_t sslen;
        size_t remote_addr_len = SIZE_MAX;
        char remote_addr[NI_MAXHOST];
        peer = ATOM_undefined;
        if ((sslen = req->conn->callbacks->get_peername(req->conn, (void *)&ss)) != 0) {
            if ((remote_addr_len = h2o_socket_getnumerichost((void *)&ss, sslen, remote_addr)) != SIZE_MAX) {
                peer = enif_make_tuple2(env, enif_make_string_len(env, remote_addr, remote_addr_len, ERL_NIF_LATIN1),
                                        enif_make_long(env, h2o_socket_getport((void *)&ss)));
            }
        }
        tuple[i++] = peer;
    }
    /* port */
    tuple[i++] = enif_make_uint(env, req->hostconf->authority.port);
    /* res */
    {
        ERL_NIF_TERM stuple[5];
        size_t j;
        j = 0;
        /* record: h2o_res */
        stuple[j++] = ATOM_h2o_res;
        /* status */
        stuple[j++] = enif_make_int(env, req->res.status);
        /* reason */
        if (req->res.reason == NULL) {
            stuple[j++] = ATOM_undefined;
        } else {
            h2o_iovec_t reason = h2o_iovec_init(req->res.reason, strlen(req->res.reason));
            h2o_iovec_to_binary(reason, &tmp);
            stuple[j++] = tmp;
        }
        /* content_length */
        stuple[j++] = (req->res.content_length == SIZE_MAX) ? ATOM_undefined : enif_make_ulong(env, req->res.content_length);
        /* headers */
        {
            h2o_header_t header;
            ERL_NIF_TERM slist[req->res.headers.size];
            size_t k;
            for (k = 0; k < req->res.headers.size; k++) {
                header = req->res.headers.entries[k];
                buf = enif_make_new_binary(env, header.name->len, &key);
                (void)memcpy(buf, header.name->base, header.name->len);
                buf = enif_make_new_binary(env, header.value.len, &val);
                (void)memcpy(buf, header.value.base, header.value.len);
                slist[k] = enif_make_tuple2(env, key, val);
            }
            tmp = enif_make_list_from_array(env, slist, req->headers.size);
            stuple[j++] = tmp;
        }
        tuple[i++] = enif_make_tuple_from_array(env, stuple, j);
    }
    /* resp_body */
    tuple[i++] = ATOM_undefined;
    /* resp_cookies */
    tuple[i++] = ATOM_undefined;
    /* resp_headers */
    tuple[i++] = ATOM_undefined;
    /* scheme */
    h2o_iovec_to_binary(req->scheme->name, &tmp);
    tuple[i++] = tmp;
    /* send_state */
    tuple[i++] = ATOM_in_progress;
    /* streamid */
    tuple[i++] = (stream == NULL) ? ATOM_undefined : enif_make_ulong(env, stream->stream_id);
    /* version */
    tuple[i++] = h2o_req_version_to_atom(req);

#undef h2o_iovec_to_binary

    return enif_make_tuple_from_array(env, tuple, i);
}

/* Port Functions */

static ERL_NIF_TERM h2o_nif_filter_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_filter_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

int
h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot, h2o_nif_filter_event_t **eventp)
{
    TRACE_F("h2o_nif_filter_event_open:%s:%d\n", __FILE__, __LINE__);
    assert(eventp != NULL);
    h2o_nif_filter_event_t *event = NULL;
    if (!h2o_nif_port_open(&filter->super, sizeof(h2o_nif_filter_event_t), (h2o_nif_port_t **)&event)) {
        *eventp = NULL;
        return 0;
    }
    event->super.dtor = h2o_nif_filter_event_dtor;
    event->super.type = H2O_NIF_PORT_TYPE_FILTER_EVENT;
    event->_link.prev = event->_link.next = NULL;
    event->req = req;
    (void)atomic_init(&event->entity_offset, 0);
    (void)ck_spinlock_init(&event->state.lock);
    (void)atomic_init(&event->state.skip, 0);
    (void)h2o_linklist_init_anchor(&event->state.input);
    (void)atomic_init(&event->state.num_input, 0);
    event->state.ready_input = (atomic_flag)ATOMIC_FLAG_INIT;
    event->state.input_state = H2O_SEND_STATE_IN_PROGRESS;
    (void)h2o_linklist_init_anchor(&event->state.output);
    (void)atomic_init(&event->state.num_output, 0);
    event->state.ready_output = (atomic_flag)ATOMIC_FLAG_INIT;
    event->state.output_state = H2O_SEND_STATE_IN_PROGRESS;
    (void)atomic_init(&event->state.output_batch, (uintptr_t)NULL);
    event->super.on_close.callback = h2o_nif_filter_event_on_close;
    h2o_nif_filter_ostream_t *ostream = (void *)h2o_add_ostream(req, sizeof(h2o_nif_filter_ostream_t), slot);
    if (ostream == NULL) {
        (void)h2o_nif_port_close(&event->super, NULL, NULL);
        *eventp = NULL;
        return 0;
    }
    ostream->super.do_send = on_send;
    ostream->super.stop = on_stop;
    slot = &ostream->super.next;
    (void)h2o_nif_port_keep(&event->super);
    (void)atomic_init(&ostream->event, (uintptr_t)event);
    (void)atomic_init(&event->ostream, (uintptr_t)ostream);
    (void)h2o_setup_next_ostream(req, slot);
    (void)h2o_nif_port_keep(&event->super);
    *eventp = event;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_filter_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_filter_event_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT);
    h2o_nif_filter_event_t *event = (h2o_nif_filter_event_t *)port;
    // h2o_nif_filter_ostream_t *ostream = (h2o_nif_filter_ostream_t *)atomic_exchange_explicit(&event->ostream, (uintptr_t)NULL,
    // memory_order_relaxed);
    // if (ostream != NULL) {
    //     if (atomic_compare_exchange_weak_explicit(&ostream->event, (uintptr_t *)&event, (uintptr_t)NULL, memory_order_relaxed,
    //     memory_order_relaxed)) {
    //         (void)h2o_nif_port_release(&event->super);
    //     }
    // }
    // if (h2o_timeout_is_linked(&event->state.output_timeout)) {
    //     (void)h2o_timeout_unlink(&event->state.output_timeout);
    // }
    (void)h2o_nif_port_release(&event->super);
    return ATOM_ok;
}

static void
h2o_nif_filter_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_filter_event_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT);
    return;
}

/* Filter Functions */

static void
on_send(h2o_ostream_t *super, h2o_req_t *req, h2o_iovec_t *inbufs, size_t inbufcnt, h2o_send_state_t state)
{
    TRACE_F("on_send:%s:%d\n", __FILE__, __LINE__);

    h2o_nif_filter_ostream_t *ostream = (h2o_nif_filter_ostream_t *)super;
    h2o_nif_filter_event_t *event = (h2o_nif_filter_event_t *)atomic_load_explicit(&ostream->event, memory_order_relaxed);
    size_t i;
    size_t inbuflen = 0;

    if (atomic_load_explicit(&event->state.skip, memory_order_relaxed) == 1) {
        (void)h2o_ostream_send_next(&ostream->super, req, inbufs, inbufcnt, state);
        return;
    }

    for (i = 0; i < inbufcnt; ++i) {
        inbuflen += inbufs[i].len;
    }

    // TRACE_F("req->res.content_length = %lu\n", req->res.content_length);
    // req->res.content_length = SIZE_MAX;

    /* emit */
    {
        ErlNifBinary binary;
        h2o_nif_filter_input_t *input = NULL;
        unsigned char *buf = NULL;
        if (!enif_alloc_binary(sizeof(h2o_nif_filter_input_t) + inbuflen, &binary)) {
            perror("unable to allocate filter input binary");
            abort();
        }
        input = (h2o_nif_filter_input_t *)binary.data;
        input->_link.next = input->_link.prev = NULL;
        input->binary = binary;
        buf = binary.data + sizeof(h2o_nif_filter_input_t);
        for (i = 0; i < inbufcnt; ++i) {
            (void)memcpy(buf, inbufs[i].base, inbufs[i].len);
            buf += inbufs[i].len;
        }
        (void)atomic_fetch_add_explicit(&event->state.num_input, 1, memory_order_relaxed);
        (void)ck_spinlock_lock_eb(&event->state.lock);
        (void)h2o_linklist_insert(&event->state.input, &input->_link);
        event->state.input_state = state;
        (void)ck_spinlock_unlock(&event->state.lock);
    }
    if (!atomic_flag_test_and_set_explicit(&event->state.ready_input, memory_order_relaxed)) {
        // __deferred_action_t action;
        // (void)memset(&action, 0, sizeof(action));
        // action.timeout.registered_at = 0;
        // action.timeout.cb = on_ready_input_cb;
        // action.timeout._link.next = action.timeout._link.prev = NULL;
        // action.event = event;
        // action.req = event->req;
        // on_ready_input_cb(&action.timeout);
        (void)create_deferred_action(event, req, on_ready_input_cb);
    }
    // if (state == H2O_SEND_STATE_FINAL) {
    //     // __deferred_action_t action;
    //     // (void)memset(&action, 0, sizeof(action));
    //     // action.timeout.registered_at = 0;
    //     // action.timeout.cb = on_final_input_cb;
    //     // action.timeout._link.next = action.timeout._link.prev = NULL;
    //     // action.event = event;
    //     // action.req = event->req;
    //     // on_final_input_cb(&action.timeout);
    //     (void)create_deferred_action(event, req, on_final_input_cb);
    // }
}

static void
on_stop(h2o_ostream_t *super, h2o_req_t *req)
{
    TRACE_F("on_stop:%s:%d\n", __FILE__, __LINE__);

    h2o_nif_filter_ostream_t *ostream = (h2o_nif_filter_ostream_t *)super;
    h2o_nif_filter_event_t *event =
        (h2o_nif_filter_event_t *)atomic_exchange_explicit(&ostream->event, (uintptr_t)NULL, memory_order_relaxed);
    if (event != NULL) {
        if (h2o_timeout_is_linked(&event->state.output_timeout)) {
            (void)h2o_timeout_unlink(&event->state.output_timeout);
        }
        (void)atomic_compare_exchange_weak_explicit(&event->ostream, (uintptr_t *)&ostream, (uintptr_t)NULL, memory_order_relaxed,
                                                    memory_order_relaxed);
        (void)h2o_nif_port_release(&event->super);
    }
}

static __deferred_action_t *
create_deferred_action(h2o_nif_filter_event_t *event, h2o_req_t *req, h2o_timeout_cb cb)
{
    TRACE_F("create_deferred_action:%s:%d\n", __FILE__, __LINE__);

    __deferred_action_t *action = h2o_mem_alloc_shared(&req->pool, sizeof(*action), on_deferred_action_dispose);
    (void)h2o_nif_port_keep(&event->super);
    *action = (__deferred_action_t){{0, cb}, event, req};
    (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &action->timeout);
    return action;
}

static void
on_deferred_action_dispose(void *_action)
{
    TRACE_F("on_deferred_action_dispose:%s:%d\n", __FILE__, __LINE__);

    __deferred_action_t *action = _action;
    if (h2o_timeout_is_linked(&action->timeout)) {
        (void)h2o_timeout_unlink(&action->timeout);
        (void)h2o_nif_port_release(&action->event->super);
    }
}

static int send_em(ErlNifEnv *env, h2o_nif_filter_event_t *filter_event);

static void
on_ready_input_cb(h2o_timeout_entry_t *entry)
{
    TRACE_F("on_ready_input_cb:%s:%d\n", __FILE__, __LINE__);

    __deferred_action_t *action = H2O_STRUCT_FROM_MEMBER(__deferred_action_t, timeout, entry);
    h2o_nif_filter_event_t *event = action->event;
    // h2o_nif_filter_t *filter = (h2o_nif_filter_t *)event->super.parent;
    // h2o_nif_filter_ctx_t *ctx = (h2o_nif_filter_ctx_t *)atomic_load_explicit(&filter->ctx, memory_order_relaxed);
    h2o_req_t *req = action->req;
    // h2o_nif_filter_data_t *filter_data = h2o_context_get_filter_context(req->conn->ctx, &ctx->super);
    // ErlNifEnv *env = filter_data->env;
    ErlNifEnv *env = enif_alloc_env();
    ERL_NIF_TERM msg;
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &event->super), ATOM_ready_input);
    if (!h2o_nif_port_send(NULL, &event->super, NULL, msg)) {
        (void)atomic_flag_clear_explicit(&event->state.ready_input, memory_order_relaxed);
    }
    (void)send_em(env, event);
    if (event->state.input_state == H2O_SEND_STATE_FINAL) {
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &event->super), ATOM_final_input);
        (void)h2o_nif_port_send(NULL, &event->super, NULL, msg);
    }
    (void)enif_free_env(env);
    (void)h2o_nif_port_release(&event->super);
}

static int
send_em(ErlNifEnv *env, h2o_nif_filter_event_t *filter_event)
{
    h2o_req_t *req = filter_event->req;
    ERL_NIF_TERM msg;

    h2o_linklist_t input;
    size_t num_input;
    (void)atomic_flag_clear_explicit(&filter_event->state.ready_input, memory_order_relaxed);
    (void)h2o_linklist_init_anchor(&input);
    num_input = atomic_load_explicit(&filter_event->state.num_input, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&filter_event->state.lock);
    (void)h2o_linklist_insert_list(&input, &filter_event->state.input);
    (void)ck_spinlock_unlock(&filter_event->state.lock);
    if (h2o_linklist_is_empty(&input)) {
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
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter_event->super), list);
    int retval = h2o_nif_port_send(NULL, &filter_event->super, NULL, msg);
    (void)enif_clear_env(env);
    return retval;
}

// static void
// on_final_input_cb(h2o_timeout_entry_t *entry)
// {
//     TRACE_F("on_final_input_cb:%s:%d\n", __FILE__, __LINE__);

//     __deferred_action_t *action = H2O_STRUCT_FROM_MEMBER(__deferred_action_t, timeout, entry);
//     h2o_nif_filter_event_t *event = action->event;
//     h2o_nif_filter_t *filter = (h2o_nif_filter_t *)event->super.parent;
//     h2o_nif_filter_ctx_t *ctx = (h2o_nif_filter_ctx_t *)atomic_load_explicit(&filter->ctx, memory_order_relaxed);
//     h2o_req_t *req = action->req;
//     h2o_nif_filter_data_t *filter_data = h2o_context_get_filter_context(req->conn->ctx, &ctx->super);
//     ErlNifEnv *env = filter_data->env;
//     ERL_NIF_TERM msg;
//     msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &event->super), ATOM_final_input);
//     (void)h2o_nif_port_send(NULL, &event->super, env, msg);
//     (void)enif_clear_env(env);
// }
