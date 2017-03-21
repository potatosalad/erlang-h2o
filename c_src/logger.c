// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "logger.h"

static void on_dispose(h2o_logger_t *super);
static void on_log_access(h2o_logger_t *super, h2o_req_t *req);
static void on_context_init(h2o_logger_t *super, h2o_context_t *context);
static void on_context_dispose(h2o_logger_t *super, h2o_context_t *context);

/* Types */

typedef struct h2o_nif_logger_data_s h2o_nif_logger_data_t;

struct h2o_nif_logger_data_s {
    ErlNifEnv *env;
};

/* Port Functions */

static int h2o_nif_logger_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh, h2o_nif_logger_t **loggerp);
static ERL_NIF_TERM h2o_nif_logger_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_logger_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static int
h2o_nif_logger_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh, h2o_nif_logger_t **loggerp)
{
    assert(loggerp != NULL);
    h2o_nif_logger_t *logger = NULL;
    if (!h2o_nif_port_open(&server->super, sizeof(h2o_nif_logger_t), (h2o_nif_port_t **)&logger)) {
        *loggerp = NULL;
        return 0;
    }
    logger->super.dtor = h2o_nif_logger_dtor;
    logger->super.type = H2O_NIF_PORT_TYPE_LOGGER;
    (void)atomic_init(&logger->ctx, (uintptr_t)NULL);
    (void)h2o_mem_addref_shared(lh);
    logger->lh = lh;
    (void)atomic_init(&logger->state, H2O_NIF_LOGGER_STATE_PASSIVE);
    (void)ck_spinlock_init(&logger->spinlock);
    (void)h2o_linklist_init_anchor(&logger->events);
    (void)atomic_init(&logger->num_events, 0);
    /* create logger context */
    h2o_nif_logger_ctx_t *ctx = (h2o_nif_logger_ctx_t *)h2o_create_logger(pathconf, sizeof(*ctx));
    if (ctx == NULL) {
        (void)h2o_nif_port_close(&logger->super, NULL, NULL);
        *loggerp = NULL;
        return 0;
    }
    (void)h2o_nif_port_keep(&logger->super);
    (void)atomic_store_explicit(&ctx->logger, (uintptr_t)logger, memory_order_relaxed);
    (void)atomic_store_explicit(&logger->ctx, (uintptr_t)ctx, memory_order_relaxed);
    ctx->super._config_slot = pathconf->global->_num_config_slots++;
    ctx->super.dispose = on_dispose;
    ctx->super.log_access = on_log_access;
    ctx->super.on_context_init = on_context_init;
    ctx->super.on_context_dispose = on_context_dispose;
    (void)h2o_nif_port_keep(&logger->super);
    logger->super.on_close.callback = h2o_nif_logger_on_close;
    *loggerp = logger;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_logger_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_logger_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_LOGGER);
    h2o_nif_logger_t *logger = (h2o_nif_logger_t *)port;
    h2o_nif_logger_ctx_t *ctx = (h2o_nif_logger_ctx_t *)atomic_exchange_explicit(&logger->ctx, (uintptr_t)NULL, memory_order_relaxed);
    if (ctx != NULL) {
        if (atomic_compare_exchange_weak_explicit(&ctx->logger, (uintptr_t *)&logger, (uintptr_t)NULL, memory_order_relaxed, memory_order_relaxed)) {
            (void)h2o_nif_port_release(&logger->super);
        }
    }
    (void)h2o_nif_port_release(&logger->super);
    return ATOM_ok;
}

static void
h2o_nif_logger_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_logger_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_LOGGER);
    h2o_nif_logger_t *logger = (h2o_nif_logger_t *)port;
    (void)h2o_mem_release_shared(logger->lh);
    return;
}

/* Logger Functions */

h2o_nif_logger_ctx_t *
h2o_nif_logger_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_logger_handle_t *lh)
{
    TRACE_F("h2o_nif_logger_register:%s:%d\n", __FILE__, __LINE__);
    assert(pathconf != NULL);
    h2o_nif_logger_t *logger = NULL;
    if (!h2o_nif_logger_open(server, pathconf, lh, &logger)) {
        return NULL;
    }
    if (!h2o_nif_port_set_listening(&logger->super)) {
        (void)h2o_nif_port_close(&logger->super, NULL, NULL);
        return NULL;
    }
    ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, lh->reference), h2o_nif_port_make(env, &logger->super));
    (void)h2o_nif_port_send(env, &logger->super, NULL, msg);
    return ((h2o_nif_logger_ctx_t *)atomic_load_explicit(&logger->ctx, memory_order_relaxed));
}

static void
on_dispose(h2o_logger_t *super)
{
    h2o_nif_logger_ctx_t *ctx = (h2o_nif_logger_ctx_t *)super;
    h2o_nif_logger_t *logger = (h2o_nif_logger_t *)atomic_exchange_explicit(&ctx->logger, (uintptr_t)NULL, memory_order_relaxed);
    if (logger != NULL) {
        (void)atomic_compare_exchange_weak_explicit(&ctx->logger, (uintptr_t *)&logger, (uintptr_t)NULL, memory_order_relaxed, memory_order_relaxed);
        (void)h2o_nif_port_release(&logger->super);
    }
}

static void
on_log_access(h2o_logger_t *super, h2o_req_t *req)
{
    TRACE_F("on_log_access:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_logger_ctx_t *ctx = (h2o_nif_logger_ctx_t *)super;
    if (ctx == NULL) {
        return;
    }
    h2o_nif_logger_t *logger = (h2o_nif_logger_t *)atomic_load_explicit(&ctx->logger, memory_order_relaxed);
    if (logger == NULL) {
        return;
    }

    char *logline;
    char buf[4096];
    size_t len;
    // ErlNifEnv *env = enif_alloc_env();
    // ERL_NIF_TERM msg;

    /* stringify */
    len = sizeof(buf);
    logline = h2o_log_request(logger->lh->logconf, req, &len, buf);

    /* emit */
    {
        ErlNifBinary tmpbin;
        h2o_nif_logger_event_t *event;
        (void)enif_alloc_binary(sizeof(h2o_nif_logger_event_t) + len, &tmpbin);
        // TRACE_F("binary size: %lu -> %lu\n", len, tmpbin.size);
        event = (h2o_nif_logger_event_t *)tmpbin.data;
        event->_link.next = event->_link.prev = NULL;
        event->binary = tmpbin;
        // event->binary.data = tmpbin.data + sizeof(h2o_nif_logger_event_t);
        // event->binary.size = len;
        (void)memcpy(tmpbin.data + sizeof(h2o_nif_logger_event_t), logline, len);
        (void)atomic_fetch_add_explicit(&logger->num_events, 1, memory_order_relaxed);
        (void)ck_spinlock_lock_eb(&logger->spinlock);
        (void)h2o_linklist_insert(&logger->events, &event->_link);
        (void)ck_spinlock_unlock(&logger->spinlock);
    }
    {
        int expected = H2O_NIF_LOGGER_STATE_NOTIFY;
        int desired = H2O_NIF_LOGGER_STATE_ACTIVE;
        // TRACE_F("logger->state: %d\n", atomic_load_explicit(&logger->state, memory_order_relaxed));
        // ERL_NIF_TERM out
        // unsigned char *out_buf = enif_make_new_binary()
        if (atomic_compare_exchange_weak_explicit(&logger->state, &expected, desired, memory_order_relaxed, memory_order_relaxed)) {
            h2o_nif_logger_data_t *logger_data = h2o_context_get_logger_context(req->conn->ctx, super);
            ErlNifEnv *env = logger_data->env;
            ERL_NIF_TERM msg;
            msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &logger->super), ATOM_ready_input);
            if (!h2o_nif_port_send(NULL, &logger->super, env, msg)) {
                expected = H2O_NIF_LOGGER_STATE_ACTIVE;
                desired = H2O_NIF_LOGGER_STATE_NOTIFY;
                (void)atomic_compare_exchange_weak_explicit(&logger->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
            }
            (void)enif_clear_env(env);
        }
    }

    // unsigned char *msg_buf = enif_make_new_binary(env, len, &msg);
    // (void)memcpy(msg_buf, logline, len);
    // msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &logger->super), msg);
    // (void)h2o_nif_port_send(NULL, &logger->super, env, msg);
    // (void)enif_free_env(env);

    /* free memory */
    if (logline != buf) {
        (void)free(logline);
    }

    return;
}

static void
on_context_init(h2o_logger_t *super, h2o_context_t *context)
{
    h2o_nif_logger_data_t *data = enif_alloc(sizeof(*data));
    data->env = enif_alloc_env();
    (void)h2o_context_set_logger_context(context, super, data);
}

static void
on_context_dispose(h2o_logger_t *super, h2o_context_t *context)
{
    h2o_nif_logger_data_t *data = h2o_context_get_logger_context(context, super);
    (void)enif_free_env(data->env);
    (void)enif_free(data);
}
