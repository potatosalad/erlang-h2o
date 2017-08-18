// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "filter_event.h"
#include <h2o.h>
#include <h2o/configurator.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <h2o/http2_internal.h>
#include <h2o/serverutil.h>
#include "sds.h"

/* Resource Functions */

#include "terms/filter_event.c.h"

/* Port Functions */

static ERL_NIF_TERM h2o_nif_filter_event_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_filter_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static h2o_nif_port_init_t h2o_nif_filter_event_init = {
    .type = H2O_NIF_PORT_TYPE_FILTER_EVENT, .stop = h2o_nif_filter_event_stop, .dtor = h2o_nif_filter_event_dtor,
};

static void on_rx_send(h2o_ostream_t *super, h2o_req_t *req, h2o_iovec_t *inbufs, size_t inbufcnt, h2o_send_state_t state);
static void on_rx_fire(h2o_timeout_entry_t *timeout_entry);
static void on_rx_stop(h2o_ostream_t *super, h2o_req_t *req);
// static void on_rx_start_pull(h2o_ostream_t *super, h2o_ostream_pull_cb cb);
static void on_tx_send(h2o_nif_ipc_receiver_t *receiver, h2o_linklist_t *messages);
static void on_tx_stop(h2o_nif_ipc_receiver_t *receiver);

static void on_request_work(h2o_nif_ipc_receiver_t *receiver, h2o_linklist_t *messages);
static void on_request_stop(h2o_nif_ipc_receiver_t *receiver);

static void on_sentinel_dtor(h2o_nif_port_t *port);

int
h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot, h2o_nif_filter_event_t **filter_eventp)
{
    TRACE_F("h2o_nif_filter_event_open:%s:%d\n", __FILE__, __LINE__);
    assert(filter_eventp != NULL);
    h2o_nif_filter_event_t *filter_event = NULL;
    if (!h2o_nif_port_open(&filter->super, &h2o_nif_filter_event_init, sizeof(h2o_nif_filter_event_t),
                           (h2o_nif_port_t **)&filter_event)) {
        *filter_eventp = NULL;
        return 0;
    }
    (void)ck_spinlock_init(&filter_event->rx.lock);
    (void)h2o_linklist_init_anchor(&filter_event->rx.queue);
    filter_event->rx.size = 0;
    filter_event->rx.state = H2O_SEND_STATE_IN_PROGRESS;
    filter_event->rx.flag = (atomic_flag)ATOMIC_FLAG_INIT;
    (void)ck_spinlock_init(&filter_event->tx.lock);
    (void)h2o_linklist_init_anchor(&filter_event->tx.queue);
    filter_event->tx.size = 0;
    filter_event->tx.state = H2O_SEND_STATE_IN_PROGRESS;
    filter_event->tx.flag = (atomic_flag)ATOMIC_FLAG_INIT;
    filter_event->tx.message.link.prev = filter_event->tx.message.link.next = NULL;
    (void)h2o_nif_ipc_link_ostream(&filter_event->ostream, req, slot, on_tx_send, on_tx_stop);
    filter_event->ostream.super.do_send = on_rx_send;
    filter_event->ostream.super.stop = on_rx_stop;
    // filter_event.ostream.super.start_pull = on_rx_start_pull;
    slot = &filter_event->ostream.super.next;
    (void)h2o_setup_next_ostream(req, slot);
    (void)h2o_nif_ipc_link_request(&filter_event->request, req, on_request_work, on_request_stop);
    (void)h2o_nif_proto_req_init(&filter_event->super, &filter_event->proto);
    assert(h2o_nif_port_sentinel_link(&req->pool, &filter_event->super, on_sentinel_dtor));
    *filter_eventp = filter_event;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_filter_event_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_filter_event_stop:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT);
    // h2o_nif_filter_event_t *event = (h2o_nif_filter_event_t *)port;
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
    // (void)h2o_nif_port_release(&event->super);
    return ATOM_ok;
}

static void
h2o_nif_filter_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_filter_event_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT);
    return;
}

static void
on_sentinel_dtor(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    TRACE_F("on_sentinel_dtor:%s:%d ENTER flags=%d, async=%d\n", __FILE__, __LINE__, state.flags, state.async);
    if (h2o_nif_port_is_finalized(port)) {
        (void)h2o_nif_port_stop_quiet(port, NULL, NULL);
    } else {
        (void)h2o_nif_port_stop(port, NULL, NULL);
    }
    state = atomic_load_explicit(&port->state, memory_order_relaxed);
    TRACE_F("on_sentinel_dtor:%s:%d EXIT flags=%d, async=%d\n", __FILE__, __LINE__, state.flags, state.async);
}

/* Filter Event Functions */

int
h2o_nif_filter_event_send(h2o_nif_filter_event_t *filter_event, h2o_iovec_t *bufs, size_t bufcnt, h2o_send_state_t state)
{
    TRACE_F("h2o_nif_filter_event_send:%s:%d\n", __FILE__, __LINE__);
    if ((state == H2O_SEND_STATE_IN_PROGRESS && !h2o_nif_port_add_send_data(&filter_event->super)) ||
        (state == H2O_SEND_STATE_FINAL && !h2o_nif_port_add_finalized(&filter_event->super))) {
        TRACE_F("unable to add send_data or finalized: %d\n", state);
        return 0;
    }
    if (!h2o_nif_port_sentinel_trylock(&filter_event->super)) {
        TRACE_F("unable to lock the sentinel\n");
        return 0;
    }
    h2o_req_t *req = filter_event->ostream.req;
    // h2o_nif_filter_event_oentry_t *fo = (void *)mem_alloc(sizeof(h2o_nif_filter_event_oentry_t));
    h2o_nif_filter_event_oentry_t *fo = (void *)h2o_mem_alloc_pool(&req->pool, sizeof(h2o_nif_filter_event_oentry_t));
    void *p = NULL;
    size_t buflen;
    size_t i;
    assert(fo != NULL);
    fo->_link.prev = fo->_link.next = NULL;
    fo->data.entries = NULL;
    fo->data.size = 0;
    fo->data.capacity = 0;
    fo->state = state;
    buflen = 0;
    for (i = 0; i < bufcnt; ++i) {
        buflen += bufs[i].len;
    }
    // DEBUG_F("event_send :: reserving %lu buffers\n", bufcnt);
    (void)h2o_vector_reserve(&req->pool, &fo->data, bufcnt);
    // DEBUG_F("event_send :: reserving %lu bytes\n", buflen);
    p = h2o_mem_alloc_pool(&req->pool, buflen);
    for (i = 0; i < bufcnt; ++i) {
        fo->data.entries[i] = h2o_iovec_init(p, bufs[i].len);
        p += bufs[i].len;
        (void)memcpy(fo->data.entries[i].base, bufs[i].base, bufs[i].len);
        fo->data.size++;
    }
    (void)ck_spinlock_lock_eb(&filter_event->tx.lock);
    (void)h2o_linklist_insert(&filter_event->tx.queue, &fo->_link);
    filter_event->tx.size++;
    filter_event->tx.state = state;
    (void)ck_spinlock_unlock(&filter_event->tx.lock);
    (void)h2o_nif_port_sentinel_tryunlock(&filter_event->super);

    // DEBUG_F("event_send :: filter_event->tx.size = %lu\n", filter_event->tx.size);
    if (!atomic_flag_test_and_set_explicit(&filter_event->tx.flag, memory_order_relaxed)) {
        assert(!h2o_linklist_is_linked(&filter_event->tx.message.link));
        // TRACE_F("about to send: %d\n", state);
        if (h2o_nif_ipc_send(&filter_event->ostream.receiver, &filter_event->tx.message)) {
            return 1;
        }
    }
    (void)h2o_nif_port_sub_requested(&filter_event->super);
    return 1;
}

/* Output Stream Functions */

static void
on_rx_send(h2o_ostream_t *super, h2o_req_t *req, h2o_iovec_t *inbufs, size_t inbufcnt, h2o_send_state_t state)
{
    TRACE_F("on_rx_send:%s:%d \"%.*s\"\n", __FILE__, __LINE__, (inbufcnt > 0) ? inbufs[0].len : 0, (inbufcnt > 0) ? inbufs[0].base : "");
    h2o_nif_ipc_ostream_t *ostream = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_ostream_t, super, super);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, ostream, ostream);
    size_t i;
    size_t inbuflen = 0;
    h2o_nif_filter_event_ientry_t *fi = NULL;
    unsigned char *buf = NULL;
    for (i = 0; i < inbufcnt; ++i) {
        inbuflen += inbufs[i].len;
    }
    fi = (void *)mem_alloc(sizeof(h2o_nif_filter_event_ientry_t));
    if (fi == NULL) {
        perror("unable to allocate filter event (h2o_nif_filter_event_ientry_t)");
        abort();
    }
    fi->_link.prev = fi->_link.next = NULL;
    // DEBUG_F("enif_alloc_binary(%lu):%s:%d\n", inbuflen, __FILE__, __LINE__);
    if (!enif_alloc_binary(inbuflen, &fi->binary)) {
        perror("unable to allocate filter event (ErlNifBinary)");
        abort();
    }
    buf = fi->binary.data;
    for (i = 0; i < inbufcnt; ++i) {
        (void)memcpy(buf, inbufs[i].base, inbufs[i].len);
        buf += inbufs[i].len;
    }
    (void)ck_spinlock_lock_eb(&filter_event->rx.lock);
    (void)h2o_linklist_insert(&filter_event->rx.queue, &fi->_link);
    filter_event->rx.size++;
    filter_event->rx.state = state;
    (void)ck_spinlock_unlock(&filter_event->rx.lock);
    if (!atomic_flag_test_and_set_explicit(&filter_event->rx.flag, memory_order_relaxed)) {
        TRACE_F("rx.flag set\n");
        if (!h2o_nif_port_add_requested(&filter_event->super)) {
            h2o_nif_port_state_t pstate = atomic_load_explicit(&filter_event->super.state, memory_order_relaxed);
            TRACE_F("unable to add requested, state is currently: flags=%d, async=%d\n", pstate.flags, pstate.async);
            return;
        }
        TRACE_F("enqueueing ready_input\n");
        // assert(!h2o_linklist_is_linked(&filter_event->rx.message.link));
        // (void)h2o_nif_ipc_send(&filter_event->ostream->receiver, &filter_event->tx.)
        assert(!h2o_timeout_is_linked(&filter_event->rx.timeout_entry));
        filter_event->rx.timeout_entry = (h2o_timeout_entry_t){0, on_rx_fire, {NULL, NULL}};
        (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &filter_event->rx.timeout_entry);
    } else {
        TRACE_F("rx.flag is already set?\n");
    }
}

static void
on_rx_fire(h2o_timeout_entry_t *timeout_entry)
{
    TRACE_F("on_rx_fire:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, rx.timeout_entry, timeout_entry);
    assert(!h2o_timeout_is_linked(&filter_event->rx.timeout_entry));
    ErlNifEnv *env = enif_alloc_env();
    assert(env != NULL);
    ERL_NIF_TERM msg;
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter_event->super), ATOM_ready_input);
    if (!h2o_nif_port_send(NULL, &filter_event->super, env, msg)) {
        (void)atomic_flag_clear_explicit(&filter_event->rx.flag, memory_order_relaxed);
    }
    (void)enif_clear_env(env);
    if (filter_event->rx.state == H2O_SEND_STATE_FINAL) {
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter_event->super), ATOM_final_input);
        (void)h2o_nif_port_send(NULL, &filter_event->super, env, msg);
    }
    (void)enif_free_env(env);
    (void)h2o_nif_port_sub_requested(&filter_event->super);
}

static void
on_rx_stop(h2o_ostream_t *super, h2o_req_t *req)
{
    TRACE_F("on_rx_stop:%s:%d ENTER\n", __FILE__, __LINE__);
    h2o_nif_ipc_ostream_t *ostream = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_ostream_t, super, super);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, ostream, ostream);
    // (void)h2o_nif_ipc_unlink_ostream()
    (void)h2o_nif_port_stop(&filter_event->super, NULL, NULL);
    TRACE_F("on_rx_stop:%s:%d EXIT\n", __FILE__, __LINE__);
}

static void
on_tx_send(h2o_nif_ipc_receiver_t *receiver, h2o_linklist_t *messages)
{
    // DEBUG_F("on_tx_send:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_event_t *filter_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, ostream.receiver, receiver);
    h2o_req_t *req = filter_event->ostream.req;
    h2o_nif_ipc_message_t *message = NULL;
    size_t cnt = 0;

    (void)atomic_flag_clear_explicit(&filter_event->tx.flag, memory_order_relaxed);

    while (!h2o_linklist_is_empty(messages)) {
        message = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_message_t, link, messages->next);
        (void)h2o_linklist_unlink(&message->link);
        cnt++;
    }
    assert(cnt == 1);
    
    h2o_linklist_t queue;
    size_t num_queue;
    h2o_nif_filter_event_oentry_t *fo = NULL;
    size_t bufcnt;
    h2o_linklist_t *anchor = NULL;
    h2o_linklist_t *node = NULL;
    h2o_iovec_vector_t data;
    h2o_send_state_t old_state;
    h2o_send_state_t new_state;
    size_t i;
    size_t j;

    (void)h2o_linklist_init_anchor(&queue);
    (void)ck_spinlock_lock_eb(&filter_event->tx.lock);
    (void)h2o_linklist_insert_list(&queue, &filter_event->tx.queue);
    num_queue = filter_event->tx.size;
    old_state = new_state = filter_event->tx.state;
    (void)ck_spinlock_unlock(&filter_event->tx.lock);
    if (h2o_linklist_is_empty(&queue) || !h2o_nif_port_sub_send_data(&filter_event->super)) {
        DEBUG_F("unable to sub send data\n");
        return;
    }

    // DEBUG_F("on_tx_send :: num_queue = %lu\n", num_queue);

    anchor = &queue;
    node = anchor->next;
    bufcnt = 0;
    while (node != anchor) {
        fo = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_oentry_t, _link, node);
        node = node->next;
        bufcnt += fo->data.size;
    }
    data.entries = NULL;
    data.size = 0;
    data.capacity = 0;
    (void)h2o_vector_reserve(&req->pool, &data, bufcnt);
    // DEBUG_F("on_tx_send :: reserved %lu buffers\n", bufcnt);
    i = 0;
    while (!h2o_linklist_is_empty(&queue)) {
        fo = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_oentry_t, _link, queue.next);
        (void)h2o_linklist_unlink(&fo->_link);
        for (j = 0; j < fo->data.size; ++j) {
            data.entries[i] = fo->data.entries[j];
            data.size++;
            // sds mysds = sdsempty();
            // mysds = sdscatrepr(mysds, data.entries[i].base, data.entries[i].len);
            // DEBUG_F("on_tx_send :: fo->data.entries[%d] = \"%.*s\"\n", i, fo->data.entries[i].len, fo->data.entries[i].base);
            // DEBUG_F("on_tx_send :: data.entries[%d] = \"%.*s\"\n", i, data.entries[i].len, data.entries[i].base);
            // DEBUG_F("on_tx_send :: data.entries[%d] = %.*s\n", i, sdslen(mysds), mysds);
            // (void)sdsfree(mysds);
            i++;
        }
        new_state = fo->state;
        // (void)mem_free(fo);
    }

    (void)ck_spinlock_lock_eb(&filter_event->tx.lock);
    filter_event->tx.size -= num_queue;
    filter_event->tx.state = new_state;
    (void)ck_spinlock_unlock(&filter_event->tx.lock);

    // if (data.size > 0) {
    //     sds mysds = sdsempty();
    //     mysds = sdscatrepr(mysds, data.entries[0].base, data.entries[0].len);
    //     // DEBUG_F("h2o_ostream_send_next: \"%.*s\" (%d)\n", data.entries[0].len, data.entries[0].base, new_state);
    //     // DEBUG_F("h2o_ostream_send_next: %.*s (%d)\n", sdslen(mysds), mysds, new_state);
    //     (void)sdsfree(mysds);
    // }
    (void)h2o_ostream_send_next(&filter_event->ostream.super, req, data.entries, data.size, new_state);

    // if (req->_generator == NULL) {
    //     static h2o_generator_t generator = {NULL, NULL};
    //     (void)h2o_start_response(req, &generator);
    // }

    // (void)h2o_send(req, bufs, bufcnt, new_state);
}

static void
on_tx_stop(h2o_nif_ipc_receiver_t *receiver)
{
    TRACE_F("on_tx_stop:%s:%d\n", __FILE__, __LINE__);
}

/* Request Functions */

static void
on_request_work(h2o_nif_ipc_receiver_t *receiver, h2o_linklist_t *messages)
{
    TRACE_F("on_request_work:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_ipc_job_t *job = NULL;
    while (!h2o_linklist_is_empty(messages)) {
        job = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_job_t, super.link, messages->next);
        (void)h2o_linklist_unlink(&job->super.link);
        job->cb(receiver, job);
    }
}

static void
on_request_stop(h2o_nif_ipc_receiver_t *receiver)
{
    TRACE_F("on_request_stop:%s:%d\n", __FILE__, __LINE__);
}
