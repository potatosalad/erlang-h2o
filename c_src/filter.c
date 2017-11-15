// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "filter.h"

static void on_context_init(h2o_filter_t *super, h2o_context_t *context);
static void on_context_dispose(h2o_filter_t *super, h2o_context_t *context);
static void on_dispose(h2o_filter_t *super);
static void on_setup_ostream(h2o_filter_t *super, h2o_req_t *req, h2o_ostream_t **slot);

/* Types */

typedef struct h2o_nif_filter_data_s h2o_nif_filter_data_t;

struct h2o_nif_filter_data_s {
    ErlNifEnv *env;
};

/* Port Functions */

static int h2o_nif_filter_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_t **filterp);
static ERL_NIF_TERM h2o_nif_filter_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_filter_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static int h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot,
                                     h2o_nif_filter_event_t **eventp);
static ERL_NIF_TERM h2o_nif_filter_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_filter_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static int
h2o_nif_filter_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_t **filterp)
{
    assert(filterp != NULL);
    h2o_nif_filter_t *filter = NULL;
    if (!h2o_nif_port_open(&server->super, sizeof(h2o_nif_filter_t), (h2o_nif_port_t **)&filter)) {
        *filterp = NULL;
        return 0;
    }
    filter->super.dtor = h2o_nif_filter_dtor;
    filter->super.type = H2O_NIF_PORT_TYPE_FILTER;
    (void)atomic_init(&filter->ctx, (uintptr_t)NULL);
    filter->state = (atomic_flag)ATOMIC_FLAG_INIT;
    (void)ck_spinlock_init(&filter->spinlock);
    (void)h2o_linklist_init_anchor(&filter->events);
    (void)atomic_init(&filter->num_events, 0);
    /* create filter context */
    h2o_nif_filter_ctx_t *ctx = (h2o_nif_filter_ctx_t *)h2o_create_filter(pathconf, sizeof(*ctx));
    if (ctx == NULL) {
        (void)h2o_nif_port_close(&filter->super, NULL, NULL);
        *filterp = NULL;
        return 0;
    }
    (void)h2o_nif_port_keep(&filter->super);
    (void)atomic_store_explicit(&ctx->filter, (uintptr_t)filter, memory_order_relaxed);
    (void)atomic_store_explicit(&filter->ctx, (uintptr_t)ctx, memory_order_relaxed);
    ctx->super._config_slot = pathconf->global->_num_config_slots++;
    ctx->super.on_context_init = on_context_init;
    ctx->super.on_context_dispose = on_context_dispose;
    ctx->super.dispose = on_dispose;
    ctx->super.on_setup_ostream = on_setup_ostream;
    (void)h2o_nif_port_keep(&filter->super);
    filter->super.on_close.callback = h2o_nif_filter_on_close;
    *filterp = filter;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_filter_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_filter_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER);
    h2o_nif_filter_t *filter = (h2o_nif_filter_t *)port;
    h2o_nif_filter_ctx_t *ctx =
        (h2o_nif_filter_ctx_t *)atomic_exchange_explicit(&filter->ctx, (uintptr_t)NULL, memory_order_relaxed);
    if (ctx != NULL) {
        if (atomic_compare_exchange_weak_explicit(&ctx->filter, (uintptr_t *)&filter, (uintptr_t)NULL, memory_order_relaxed,
                                                  memory_order_relaxed)) {
            (void)h2o_nif_port_release(&filter->super);
        }
    }
    (void)h2o_nif_port_release(&filter->super);
    return ATOM_ok;
}

static void
h2o_nif_filter_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_filter_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER);
    h2o_nif_filter_t *filter = (h2o_nif_filter_t *)port;
    return;
}

static int
h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot, h2o_nif_filter_event_t **eventp)
{
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
    event->slot = slot;
    event->super.on_close.callback = h2o_nif_filter_event_on_close;
    *eventp = event;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_filter_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_filter_event_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT);
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

h2o_nif_filter_ctx_t *
h2o_nif_filter_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_handle_t *fh)
{
    TRACE_F("h2o_nif_filter_register:%s:%d\n", __FILE__, __LINE__);
    assert(pathconf != NULL);
    h2o_nif_filter_t *filter = NULL;
    if (!h2o_nif_filter_open(server, pathconf, &filter)) {
        return NULL;
    }
    if (!h2o_nif_port_set_listening(&filter->super)) {
        (void)h2o_nif_port_close(&filter->super, NULL, NULL);
        return NULL;
    }
    ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, fh->reference), h2o_nif_port_make(env, &filter->super));
    (void)h2o_nif_port_send(env, &filter->super, NULL, msg);
    return ((h2o_nif_filter_ctx_t *)atomic_load_explicit(&filter->ctx, memory_order_relaxed));
}

static void
on_context_init(h2o_filter_t *super, h2o_context_t *context)
{
    h2o_nif_filter_data_t *data = enif_alloc(sizeof(*data));
    data->env = enif_alloc_env();
    (void)h2o_context_set_filter_context(context, super, data);
}

static void
on_context_dispose(h2o_filter_t *super, h2o_context_t *context)
{
    h2o_nif_filter_data_t *data = h2o_context_get_filter_context(context, super);
    (void)enif_free_env(data->env);
    (void)enif_free(data);
}

static void
on_dispose(h2o_filter_t *super)
{
    h2o_nif_filter_ctx_t *ctx = (h2o_nif_filter_ctx_t *)super;
    h2o_nif_filter_t *filter = (h2o_nif_filter_t *)atomic_exchange_explicit(&ctx->filter, (uintptr_t)NULL, memory_order_relaxed);
    if (filter != NULL) {
        (void)atomic_compare_exchange_weak_explicit(&ctx->filter, (uintptr_t *)&filter, (uintptr_t)NULL, memory_order_relaxed,
                                                    memory_order_relaxed);
        (void)h2o_nif_port_release(&filter->super);
    }
}

static void
on_setup_ostream(h2o_filter_t *super, h2o_req_t *req, h2o_ostream_t **slot)
{
    TRACE_F("on_setup_ostream:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_ctx_t *ctx = (h2o_nif_filter_ctx_t *)super;
    if (ctx == NULL) {
        return;
    }
    h2o_nif_filter_t *filter = (h2o_nif_filter_t *)atomic_load_explicit(&ctx->filter, memory_order_relaxed);
    if (filter == NULL) {
        return;
    }

    /* emit */
    {
        h2o_nif_filter_event_t *event = NULL;
        if (!h2o_nif_filter_event_open(filter, req, slot, &event)) {
            perror("filter event");
            abort();
        }
        (void)atomic_fetch_add_explicit(&filter->num_events, 1, memory_order_relaxed);
        (void)ck_spinlock_lock_eb(&filter->spinlock);
        (void)h2o_linklist_insert(&filter->events, &event->_link);
        (void)ck_spinlock_unlock(&filter->spinlock);
    }
    if (!atomic_flag_test_and_set_explicit(&filter->state, memory_order_relaxed)) {
        h2o_nif_filter_data_t *filter_data = h2o_context_get_filter_context(req->conn->ctx, super);
        ErlNifEnv *env = filter_data->env;
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter->super), ATOM_ready_input);
        if (!h2o_nif_port_send(NULL, &filter->super, env, msg)) {
            (void)atomic_flag_clear_explicit(&filter->state, memory_order_relaxed);
        }
        (void)enif_clear_env(env);
    }

    return;
}
