// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "filter.h"
#include "filter_event.h"

/* Types */

typedef struct h2o_nif_filter_context_s h2o_nif_filter_context_t;
typedef struct h2o_nif_filter_entry_s h2o_nif_filter_entry_t;

struct h2o_nif_filter_context_s {
    ErlNifEnv *env;
    h2o_nif_filter_t *filter;
    h2o_timeout_entry_t timeout_entry;
};

struct h2o_nif_filter_entry_s {
    h2o_filter_t super;
    h2o_nif_filter_t *filter;
};

static void on_context_init(h2o_filter_t *super, h2o_context_t *context);
static void on_context_dispose(h2o_filter_t *super, h2o_context_t *context);
static void on_dispose(h2o_filter_t *super);
static void on_setup_ostream(h2o_filter_t *super, h2o_req_t *req, h2o_ostream_t **slot);
static void on_ready_input(h2o_timeout_entry_t *timeout_entry);

/* Port Functions */

static int h2o_nif_filter_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_t **filterp);
static ERL_NIF_TERM h2o_nif_filter_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_filter_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static h2o_nif_port_init_t h2o_nif_filter_init = {
    .type = H2O_NIF_PORT_TYPE_FILTER, .stop = h2o_nif_filter_stop, .dtor = h2o_nif_filter_dtor,
};

static int
h2o_nif_filter_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_t **filterp)
{
    TRACE_F("h2o_nif_filter_open:%s:%d\n", __FILE__, __LINE__);
    assert(filterp != NULL);
    h2o_nif_filter_t *filter = NULL;
    if (!h2o_nif_port_open(&server->super, &h2o_nif_filter_init, sizeof(h2o_nif_filter_t), (h2o_nif_port_t **)&filter)) {
        *filterp = NULL;
        return 0;
    }
    (void)ck_spinlock_init(&filter->events.lock);
    filter->events.ready_input = (atomic_flag)ATOMIC_FLAG_INIT;
    (void)h2o_linklist_init_anchor(&filter->events.input);
    atomic_init(&filter->events.size, 0);
    /* create filter entry */
    h2o_nif_filter_entry_t *filter_entry = (void *)h2o_create_filter(pathconf, sizeof(*filter_entry));
    filter_entry->super._config_slot = pathconf->global->_num_config_slots++;
    filter_entry->super.on_context_init = on_context_init;
    filter_entry->super.on_context_dispose = on_context_dispose;
    filter_entry->super.dispose = on_dispose;
    filter_entry->super.on_setup_ostream = on_setup_ostream;
    (void)h2o_nif_port_keep(&filter->super);
    filter_entry->filter = filter;
    *filterp = filter;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_filter_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_filter_stop:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER);
    // h2o_nif_filter_t *filter = (h2o_nif_filter_t *)port;
    return ATOM_ok;
}

static void
h2o_nif_filter_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_filter_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_FILTER);
    // h2o_nif_filter_t *filter = (h2o_nif_filter_t *)port;
    return;
}

/* Config Functions */

int
h2o_nif_filter_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_filter_handle_t *fh)
{
    TRACE_F("h2o_nif_filter_register:%s:%d\n", __FILE__, __LINE__);
    assert(pathconf != NULL);
    h2o_nif_filter_t *filter = NULL;
    if (!h2o_nif_filter_open(server, pathconf, &filter)) {
        return 0;
    }
    if (!h2o_nif_port_set_listening(&filter->super)) {
        (void)h2o_nif_port_stop_quiet(&filter->super, NULL, NULL);
        return 0;
    }
    ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, fh->reference), h2o_nif_port_make(env, &filter->super));
    (void)h2o_nif_port_send(env, &filter->super, NULL, msg);
    return 1;
}

static void
on_context_init(h2o_filter_t *super, h2o_context_t *context)
{
    TRACE_F("on_context_init:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_entry_t *filter_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_entry_t, super, super);
    h2o_nif_filter_t *filter = filter_entry->filter;
    h2o_nif_filter_context_t *filter_context = mem_alloc(sizeof(*filter_context));
    assert(filter_context != NULL);
    (void)memset(filter_context, 0, sizeof(*filter_context));
    filter_context->env = enif_alloc_env();
    assert(filter_context->env != NULL);
    (void)h2o_nif_port_keep(&filter->super);
    filter_context->filter = filter;
    (void)h2o_context_set_filter_context(context, &filter_entry->super, filter_context);
}

static void
on_context_dispose(h2o_filter_t *super, h2o_context_t *context)
{
    TRACE_F("on_context_dispose:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_entry_t *filter_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_entry_t, super, super);
    h2o_nif_filter_context_t *filter_context = h2o_context_get_filter_context(context, &filter_entry->super);
    h2o_nif_filter_t *filter = filter_context->filter;
    if (h2o_timeout_is_linked(&filter_context->timeout_entry)) {
        (void)h2o_timeout_unlink(&filter_context->timeout_entry);
    }
    (void)enif_free_env(filter_context->env);
    (void)h2o_nif_port_release(&filter->super);
    (void)mem_free(filter_context);
}

static void
on_dispose(h2o_filter_t *super)
{
    TRACE_F("on_dispose:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_entry_t *filter_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_entry_t, super, super);
    h2o_nif_filter_t *filter = filter_entry->filter;
    (void)h2o_nif_port_release(&filter->super);
}

static void
on_setup_ostream(h2o_filter_t *super, h2o_req_t *req, h2o_ostream_t **slot)
{
    TRACE_F("on_setup_ostream:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_entry_t *filter_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_entry_t, super, super);
    h2o_nif_filter_context_t *filter_context = h2o_context_get_filter_context(req->conn->ctx, &filter_entry->super);
    h2o_nif_filter_t *filter = filter_context->filter;
    h2o_nif_filter_event_t *filter_event = NULL;
    if (!h2o_nif_filter_event_open(filter, req, slot, &filter_event) || !h2o_nif_port_set_requested(&filter_event->super)) {
        perror("unable to open filter event");
        abort();
    }
    (void)atomic_fetch_add_explicit(&filter->events.size, 1, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&filter->events.lock);
    (void)h2o_linklist_insert(&filter->events.input, &filter_event->_link);
    (void)ck_spinlock_unlock(&filter->events.lock);
    if (!atomic_flag_test_and_set_explicit(&filter->events.ready_input, memory_order_relaxed)) {
        assert(!h2o_timeout_is_linked(&filter_context->timeout_entry));
        filter_context->timeout_entry = (h2o_timeout_entry_t){0, on_ready_input, {NULL, NULL}};
        (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &filter_context->timeout_entry);
    }
}

static void
on_ready_input(h2o_timeout_entry_t *timeout_entry)
{
    TRACE_F("on_ready_input:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_filter_context_t *filter_context = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_context_t, timeout_entry, timeout_entry);
    h2o_nif_filter_t *filter = filter_context->filter;
    ErlNifEnv *env = filter_context->env;
    ERL_NIF_TERM msg;
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter->super), ATOM_ready_input);
    if (!h2o_nif_port_send(NULL, &filter->super, env, msg)) {
        (void)atomic_flag_clear_explicit(&filter->events.ready_input, memory_order_relaxed);
    }
    (void)enif_clear_env(env);
}
