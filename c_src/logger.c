// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "logger.h"

/* Types */

typedef struct h2o_nif_logger_context_s h2o_nif_logger_context_t;
typedef struct h2o_nif_logger_entry_s h2o_nif_logger_entry_t;

struct h2o_nif_logger_context_s {
    ErlNifEnv *env;
    h2o_nif_logger_t *logger;
    h2o_timeout_entry_t timeout_entry;
};

struct h2o_nif_logger_entry_s {
    h2o_logger_t super;
    h2o_nif_logger_t *logger;
};

static void on_context_init(h2o_logger_t *super, h2o_context_t *context);
static void on_context_dispose(h2o_logger_t *super, h2o_context_t *context);
static void on_dispose(h2o_logger_t *super);
static void on_log_access(h2o_logger_t *super, h2o_req_t *req);
static void on_ready_input(h2o_timeout_entry_t *timeout_entry);

/* Resource Functions */

#include "terms/logger_event.c.h"

/* Port Functions */

static int h2o_nif_logger_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh,
                               h2o_nif_logger_t **loggerp);
static ERL_NIF_TERM h2o_nif_logger_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_logger_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static h2o_nif_port_init_t h2o_nif_logger_init = {
    .type = H2O_NIF_PORT_TYPE_LOGGER, .stop = h2o_nif_logger_stop, .dtor = h2o_nif_logger_dtor,
};

static int
h2o_nif_logger_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh, h2o_nif_logger_t **loggerp)
{
    TRACE_F("h2o_nif_logger_open:%s:%d\n", __FILE__, __LINE__);
    assert(loggerp != NULL);
    h2o_nif_logger_t *logger = NULL;
    if (!h2o_nif_port_open(&server->super, &h2o_nif_logger_init, sizeof(h2o_nif_logger_t), (h2o_nif_port_t **)&logger)) {
        *loggerp = NULL;
        return 0;
    }
    (void)h2o_mem_addref_shared(lh);
    logger->handle = lh;
    (void)ck_spinlock_init(&logger->events.lock);
    logger->events.ready_input = (atomic_flag)ATOMIC_FLAG_INIT;
    (void)h2o_linklist_init_anchor(&logger->events.input);
    atomic_init(&logger->events.size, 0);
    /* create logger entry */
    h2o_nif_logger_entry_t *logger_entry = (void *)h2o_create_logger(pathconf, sizeof(*logger_entry));
    logger_entry->super._config_slot = pathconf->global->_num_config_slots++;
    logger_entry->super.on_context_init = on_context_init;
    logger_entry->super.on_context_dispose = on_context_dispose;
    logger_entry->super.dispose = on_dispose;
    logger_entry->super.log_access = on_log_access;
    (void)h2o_nif_port_keep(&logger->super);
    logger_entry->logger = logger;
    *loggerp = logger;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_logger_stop(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_logger_stop:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_LOGGER);
    // h2o_nif_logger_t *logger = (h2o_nif_logger_t *)port;
    return ATOM_ok;
}

static void
h2o_nif_logger_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_logger_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_LOGGER);
    h2o_nif_logger_t *logger = (h2o_nif_logger_t *)port;
    (void)h2o_mem_release_shared(logger->handle);
    return;
}

/* Config Functions */

int
h2o_nif_logger_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh)
{
    TRACE_F("h2o_nif_logger_register:%s:%d\n", __FILE__, __LINE__);
    assert(pathconf != NULL);
    h2o_nif_logger_t *logger = NULL;
    if (!h2o_nif_logger_open(server, pathconf, lh, &logger)) {
        return 0;
    }
    if (!h2o_nif_port_set_listening(&logger->super)) {
        (void)h2o_nif_port_stop_quiet(&logger->super, NULL, NULL);
        return 0;
    }
    ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, lh->reference), h2o_nif_port_make(env, &logger->super));
    (void)h2o_nif_port_send(env, &logger->super, NULL, msg);
    return 1;
}

static void
on_context_init(h2o_logger_t *super, h2o_context_t *context)
{
    TRACE_F("on_context_init:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_entry_t *logger_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_entry_t, super, super);
    h2o_nif_logger_t *logger = logger_entry->logger;
    h2o_nif_logger_context_t *logger_context = mem_alloc(sizeof(*logger_context));
    assert(logger_context != NULL);
    (void)memset(logger_context, 0, sizeof(*logger_context));
    logger_context->env = enif_alloc_env();
    assert(logger_context->env != NULL);
    (void)h2o_nif_port_keep(&logger->super);
    logger_context->logger = logger;
    (void)h2o_context_set_logger_context(context, &logger_entry->super, logger_context);
}

static void
on_context_dispose(h2o_logger_t *super, h2o_context_t *context)
{
    TRACE_F("on_context_dispose:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_entry_t *logger_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_entry_t, super, super);
    h2o_nif_logger_context_t *logger_context = h2o_context_get_logger_context(context, &logger_entry->super);
    h2o_nif_logger_t *logger = logger_context->logger;
    if (h2o_timeout_is_linked(&logger_context->timeout_entry)) {
        (void)h2o_timeout_unlink(&logger_context->timeout_entry);
    }
    (void)enif_free_env(logger_context->env);
    (void)h2o_nif_port_release(&logger->super);
    (void)mem_free(logger_context);
}

static void
on_dispose(h2o_logger_t *super)
{
    TRACE_F("on_dispose:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_entry_t *logger_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_entry_t, super, super);
    h2o_nif_logger_t *logger = logger_entry->logger;
    (void)h2o_nif_port_release(&logger->super);
}

static void
on_log_access(h2o_logger_t *super, h2o_req_t *req)
{
    TRACE_F("on_log_access:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_entry_t *logger_entry = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_entry_t, super, super);
    h2o_nif_logger_context_t *logger_context = h2o_context_get_logger_context(req->conn->ctx, &logger_entry->super);
    h2o_nif_logger_t *logger = logger_context->logger;
    h2o_nif_logger_event_t *logger_event = NULL;
    logger_event = (void *)mem_alloc(sizeof(h2o_nif_logger_event_t));
    if (logger_event == NULL) {
        perror("unable to allocate logger event");
        abort();
    }
    logger_event->_link.prev = logger_event->_link.next = NULL;

    char *logline;
    char buf[4096];
    size_t len;

    /* stringify */
    len = sizeof(buf);
    logline = h2o_log_request(logger->handle->logconf, req, &len, buf);

    /* emit */
    if (!enif_alloc_binary(len, &logger_event->binary)) {
        perror("unable to allocate logger event binary");
        abort();
    }
    (void)memcpy(logger_event->binary.data, logline, len);
    (void)atomic_fetch_add_explicit(&logger->events.size, 1, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&logger->events.lock);
    (void)h2o_linklist_insert(&logger->events.input, &logger_event->_link);
    (void)ck_spinlock_unlock(&logger->events.lock);
    if (!atomic_flag_test_and_set_explicit(&logger->events.ready_input, memory_order_relaxed)) {
        assert(!h2o_timeout_is_linked(&logger_context->timeout_entry));
        logger_context->timeout_entry = (h2o_timeout_entry_t){0, on_ready_input, {NULL, NULL}};
        (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &logger_context->timeout_entry);
    }

    /* free memory */
    if (logline != buf) {
        (void)free(logline);
    }
}

static void
on_ready_input(h2o_timeout_entry_t *timeout_entry)
{
    TRACE_F("on_ready_input:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_context_t *logger_context = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_context_t, timeout_entry, timeout_entry);
    h2o_nif_logger_t *logger = logger_context->logger;
    ErlNifEnv *env = logger_context->env;
    ERL_NIF_TERM msg;
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &logger->super), ATOM_ready_input);
    if (!h2o_nif_port_send(NULL, &logger->super, env, msg)) {
        (void)atomic_flag_clear_explicit(&logger->events.ready_input, memory_order_relaxed);
    }
    (void)enif_clear_env(env);
}
