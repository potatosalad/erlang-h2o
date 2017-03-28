// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "handler.h"
#include <h2o.h>
#include <h2o/configurator.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <h2o/http2_internal.h>
#include <h2o/serverutil.h>

static void on_context_init(h2o_handler_t *super, h2o_context_t *context);
static void on_context_dispose(h2o_handler_t *super, h2o_context_t *context);
static void on_dispose(h2o_handler_t *super);
static int on_req(h2o_handler_t *super, h2o_req_t *req);

/* Types */

typedef struct h2o_nif_handler_data_s h2o_nif_handler_data_t;

struct h2o_nif_handler_data_s {
    ErlNifEnv *env;
};

typedef struct h2o_nif_handler_deferred_action_s h2o_nif_handler_deferred_action_t;

struct h2o_nif_handler_deferred_action_s {
    h2o_timeout_entry_t timeout;
    h2o_nif_handler_t *handler;
    h2o_req_t *req;
};

/* Port Functions */

static int h2o_nif_handler_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_handler_t **handlerp);
static ERL_NIF_TERM h2o_nif_handler_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_handler_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static int h2o_nif_handler_event_open(h2o_nif_handler_t *handler, h2o_req_t *req, h2o_nif_handler_event_t **eventp);
static ERL_NIF_TERM h2o_nif_handler_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_handler_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

static int
h2o_nif_handler_open(h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_handler_t **handlerp)
{
    assert(handlerp != NULL);
    h2o_nif_handler_t *handler = NULL;
    if (!h2o_nif_port_open(&server->super, sizeof(h2o_nif_handler_t), (h2o_nif_port_t **)&handler)) {
        *handlerp = NULL;
        return 0;
    }
    handler->super.dtor = h2o_nif_handler_dtor;
    handler->super.type = H2O_NIF_PORT_TYPE_HANDLER;
    (void)atomic_init(&handler->ctx, (uintptr_t)NULL);
    handler->state = (atomic_flag)ATOMIC_FLAG_INIT;
    (void)ck_spinlock_init(&handler->spinlock);
    (void)h2o_linklist_init_anchor(&handler->events);
    (void)atomic_init(&handler->num_events, 0);
    /* create handler context */
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)h2o_create_handler(pathconf, sizeof(*ctx));
    if (ctx == NULL) {
        (void)h2o_nif_port_close(&handler->super, NULL, NULL);
        *handlerp = NULL;
        return 0;
    }
    (void)h2o_nif_port_keep(&handler->super);
    (void)atomic_store_explicit(&ctx->handler, (uintptr_t)handler, memory_order_relaxed);
    (void)atomic_store_explicit(&handler->ctx, (uintptr_t)ctx, memory_order_relaxed);
    ctx->super._config_slot = pathconf->global->_num_config_slots++;
    ctx->super.on_context_init = on_context_init;
    ctx->super.on_context_dispose = on_context_dispose;
    ctx->super.dispose = on_dispose;
    ctx->super.on_req = on_req;
    // (void)h2o_nif_port_keep(&handler->super);
    handler->super.on_close.callback = h2o_nif_handler_on_close;
    *handlerp = handler;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_handler_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_handler_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_HANDLER);
    h2o_nif_handler_t *handler = (h2o_nif_handler_t *)port;
    h2o_nif_handler_ctx_t *ctx =
        (h2o_nif_handler_ctx_t *)atomic_exchange_explicit(&handler->ctx, (uintptr_t)NULL, memory_order_relaxed);
    if (ctx != NULL) {
        if (atomic_compare_exchange_weak_explicit(&ctx->handler, (uintptr_t *)&handler, (uintptr_t)NULL, memory_order_relaxed,
                                                  memory_order_relaxed)) {
            (void)h2o_nif_port_release(&handler->super);
        }
    }
    (void)h2o_nif_port_release(&handler->super);
    return ATOM_ok;
}

static void
h2o_nif_handler_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_handler_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_HANDLER);
    // h2o_nif_handler_t *handler = (h2o_nif_handler_t *)port;
    return;
}

static int
h2o_nif_handler_event_open(h2o_nif_handler_t *handler, h2o_req_t *req, h2o_nif_handler_event_t **eventp)
{
    assert(eventp != NULL);
    h2o_nif_handler_event_t *event = NULL;
    if (!h2o_nif_port_open(&handler->super, sizeof(h2o_nif_handler_event_t), (h2o_nif_port_t **)&event)) {
        *eventp = NULL;
        return 0;
    }
    event->super.dtor = h2o_nif_handler_event_dtor;
    event->super.type = H2O_NIF_PORT_TYPE_HANDLER_EVENT;
    event->_link.prev = event->_link.next = NULL;
    event->req = req;
    (void)atomic_init(&event->num_async, 0);
    (void)atomic_init(&event->entity_offset, 0);
    // (void)ck_spinlock_init(&event->entity.lock);
    // event->entity.loaded = 0;
    // event->entity.offset = 0;
    event->super.on_close.callback = h2o_nif_handler_event_on_close;
    *eventp = event;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_handler_event_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_handler_event_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_HANDLER_EVENT);
    // h2o_nif_handler_event_t *event = (h2o_nif_handler_event_t *)port;
    return ATOM_ok;
}

static void
h2o_nif_handler_event_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_handler_event_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_HANDLER_EVENT);
    return;
}

ERL_NIF_TERM
h2o_nif_handler_event_make(ErlNifEnv *env, h2o_nif_handler_event_t *event)
{
// ERL_NIF_TERM map;
// ERL_NIF_TERM tmp;
// ERL_NIF_TERM key;
// ERL_NIF_TERM val;
// unsigned char *buf = NULL;
// map = enif_make_new_map(env);
// /* event */
// assert(enif_make_map_put(env, map, enif_make_atom(env, "event"), h2o_nif_port_make(env, &event->super), &map));
// /* authority */
// buf = enif_make_new_binary(env, event->req->authority.len, &tmp);
// (void)memcpy(buf, event->req->authority.base, event->req->authority.len);
// assert(enif_make_map_put(env, map, enif_make_atom(env, "authority"), tmp, &map));
// /* headers */
// {
//     h2o_header_t header;
//     size_t i;
//     tmp = enif_make_new_map(env);
//     for (i = 0; i < event->req->headers.size; i++) {
//         header = event->req->headers.entries[i];
//         buf = enif_make_new_binary(env, header.name->len, &key);
//         (void)memcpy(buf, header.name->base, header.name->len);
//         buf = enif_make_new_binary(env, header.value.len, &val);
//         (void)memcpy(buf, header.value.base, header.value.len);
//         assert(enif_make_map_put(env, tmp, key, val, &tmp));
//     }
//     assert(enif_make_map_put(env, map, enif_make_atom(env, "headers"), tmp, &map));
// }
// /* method */
// buf = enif_make_new_binary(env, event->req->method.len, &tmp);
// (void)memcpy(buf, event->req->method.base, event->req->method.len);
// assert(enif_make_map_put(env, map, enif_make_atom(env, "method"), tmp, &map));
// /* path */
// buf = enif_make_new_binary(env, event->req->path.len, &tmp);
// (void)memcpy(buf, event->req->path.base, event->req->path.len);
// assert(enif_make_map_put(env, map, enif_make_atom(env, "path"), tmp, &map));
// /* scheme */
// buf = enif_make_new_binary(env, event->req->scheme->name.len, &tmp);
// (void)memcpy(buf, event->req->scheme->name.base, event->req->scheme->name.len);
// assert(enif_make_map_put(env, map, enif_make_atom(env, "scheme"), tmp, &map));
// /* version */
// tmp = enif_make_int(env, event->req->version);
// assert(enif_make_map_put(env, map, enif_make_atom(env, "version"), tmp, &map));
// return map;

// ERL_NIF_TERM tmp;
// ERL_NIF_TERM key;
// ERL_NIF_TERM val;
// unsigned char *buf = NULL;
// ERL_NIF_TERM list[7];
// size_t i = 0;
// /* event */
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "event"), h2o_nif_port_make(env, &event->super));
// /* authority */
// buf = enif_make_new_binary(env, event->req->authority.len, &tmp);
// (void)memcpy(buf, event->req->authority.base, event->req->authority.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "authority"), tmp);
// /* headers */
// {
//     h2o_header_t header;
//     ERL_NIF_TERM slist[event->req->headers.size];
//     size_t j;
//     for (j = 0; j < event->req->headers.size; j++) {
//         header = event->req->headers.entries[j];
//         buf = enif_make_new_binary(env, header.name->len, &key);
//         (void)memcpy(buf, header.name->base, header.name->len);
//         buf = enif_make_new_binary(env, header.value.len, &val);
//         (void)memcpy(buf, header.value.base, header.value.len);
//         slist[j] = enif_make_tuple2(env, key, val);
//     }
//     tmp = enif_make_list_from_array(env, slist, event->req->headers.size);
//     list[i++] = enif_make_tuple2(env, enif_make_atom(env, "headers"), tmp);
// }
// /* method */
// buf = enif_make_new_binary(env, event->req->method.len, &tmp);
// (void)memcpy(buf, event->req->method.base, event->req->method.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "method"), tmp);
// /* path */
// buf = enif_make_new_binary(env, event->req->path.len, &tmp);
// (void)memcpy(buf, event->req->path.base, event->req->path.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "path"), tmp);
// /* scheme */
// buf = enif_make_new_binary(env, event->req->scheme->name.len, &tmp);
// (void)memcpy(buf, event->req->scheme->name.base, event->req->scheme->name.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "scheme"), tmp);
// /* version */
// tmp = enif_make_int(env, event->req->version);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "version"), tmp);

// return enif_make_list_from_array(env, list, i);

// ERL_NIF_TERM tmp;
// ERL_NIF_TERM key;
// ERL_NIF_TERM val;
// unsigned char *buf = NULL;
// ERL_NIF_TERM list[7];
// size_t i = 0;
// /* event */
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "event"), h2o_nif_port_make(env, &event->super));
// /* authority */
// buf = enif_make_new_binary(env, event->req->authority.len, &tmp);
// (void)memcpy(buf, event->req->authority.base, event->req->authority.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "authority"), tmp);
// /* headers */
// {
//     h2o_header_t header;
//     size_t i;
//     tmp = enif_make_new_map(env);
//     for (i = 0; i < event->req->headers.size; i++) {
//         header = event->req->headers.entries[i];
//         buf = enif_make_new_binary(env, header.name->len, &key);
//         (void)memcpy(buf, header.name->base, header.name->len);
//         buf = enif_make_new_binary(env, header.value.len, &val);
//         (void)memcpy(buf, header.value.base, header.value.len);
//         assert(enif_make_map_put(env, tmp, key, val, &tmp));
//     }
//     list[i++] = enif_make_tuple2(env, enif_make_atom(env, "headers"), tmp);
// }
// /* method */
// buf = enif_make_new_binary(env, event->req->method.len, &tmp);
// (void)memcpy(buf, event->req->method.base, event->req->method.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "method"), tmp);
// /* path */
// buf = enif_make_new_binary(env, event->req->path.len, &tmp);
// (void)memcpy(buf, event->req->path.base, event->req->path.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "path"), tmp);
// /* scheme */
// buf = enif_make_new_binary(env, event->req->scheme->name.len, &tmp);
// (void)memcpy(buf, event->req->scheme->name.base, event->req->scheme->name.len);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "scheme"), tmp);
// /* version */
// tmp = enif_make_int(env, event->req->version);
// list[i++] = enif_make_tuple2(env, enif_make_atom(env, "version"), tmp);

// return enif_make_list_from_array(env, list, i);

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
    ERL_NIF_TERM tuple[20];
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
    /* resp_body */
    tuple[i++] = ATOM_undefined;
    /* resp_cookies */
    tuple[i++] = ATOM_undefined;
    /* resp_headers */
    tuple[i++] = ATOM_undefined;
    /* scheme */
    h2o_iovec_to_binary(req->scheme->name, &tmp);
    tuple[i++] = tmp;
    /* streamid */
    tuple[i++] = (stream == NULL) ? ATOM_undefined : enif_make_ulong(env, stream->stream_id);
    /* version */
    tuple[i++] = h2o_req_version_to_atom(req);

#undef h2o_iovec_to_binary

    return enif_make_tuple_from_array(env, tuple, i);

    // #{
    // ref => Ref,
    // pid => self(),
    // streamid => StreamID,
    // peer => Peer,
    // method => Method,
    // scheme => Scheme,
    // host => Host,
    // port => Port,
    // path => Path,
    // qs => Qs,
    // version => 'HTTP/2',
    // headers => Headers,
    // has_body => IsFin =:= nofin,
    // body_length => BodyLength
    // }
}

/* Handler Functions */

static h2o_nif_handler_deferred_action_t *create_deferred_action(h2o_nif_handler_t *handler, h2o_req_t *req, h2o_timeout_cb cb);
static void on_deferred_action_dispose(void *_action);
static void on_ready_input_cb(h2o_timeout_entry_t *entry);

h2o_nif_handler_ctx_t *
h2o_nif_handler_register(ErlNifEnv *env, h2o_nif_server_t *server, h2o_pathconf_t *pathconf, h2o_nif_handler_handle_t *hh)
{
    TRACE_F("h2o_nif_handler_register:%s:%d\n", __FILE__, __LINE__);
    assert(pathconf != NULL);
    h2o_nif_handler_t *handler = NULL;
    if (!h2o_nif_handler_open(server, pathconf, &handler)) {
        return NULL;
    }
    if (!h2o_nif_port_set_listening(&handler->super)) {
        (void)h2o_nif_port_close(&handler->super, NULL, NULL);
        return NULL;
    }
    ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, hh->reference), h2o_nif_port_make(env, &handler->super));
    (void)h2o_nif_port_send(env, &handler->super, NULL, msg);
    return ((h2o_nif_handler_ctx_t *)atomic_load_explicit(&handler->ctx, memory_order_relaxed));
}

static void
on_context_init(h2o_handler_t *super, h2o_context_t *context)
{
    h2o_nif_handler_data_t *data = enif_alloc(sizeof(*data));
    data->env = enif_alloc_env();
    (void)h2o_context_set_handler_context(context, super, data);
}

static void
on_context_dispose(h2o_handler_t *super, h2o_context_t *context)
{
    h2o_nif_handler_data_t *data = h2o_context_get_handler_context(context, super);
    (void)enif_free_env(data->env);
    (void)enif_free(data);
}

static void
on_dispose(h2o_handler_t *super)
{
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)super;
    h2o_nif_handler_t *handler =
        (h2o_nif_handler_t *)atomic_exchange_explicit(&ctx->handler, (uintptr_t)NULL, memory_order_relaxed);
    if (handler != NULL) {
        (void)atomic_compare_exchange_weak_explicit(&ctx->handler, (uintptr_t *)&handler, (uintptr_t)NULL, memory_order_relaxed,
                                                    memory_order_relaxed);
        (void)h2o_nif_port_release(&handler->super);
    }
}

static int
on_req(h2o_handler_t *super, h2o_req_t *req)
{
    TRACE_F("on_req:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)super;
    if (ctx == NULL) {
        return -1;
    }
    h2o_nif_handler_t *handler = (h2o_nif_handler_t *)atomic_load_explicit(&ctx->handler, memory_order_relaxed);
    if (handler == NULL) {
        return -1;
    }

    /* emit */
    {
        h2o_nif_handler_event_t *event = NULL;
        if (!h2o_nif_handler_event_open(handler, req, &event)) {
            perror("handler event");
            abort();
        }
        if (!h2o_nif_port_set_requested(&event->super)) {
            (void)h2o_nif_port_close(&event->super, NULL, NULL);
            return -1;
        }
        (void)atomic_fetch_add_explicit(&handler->num_events, 1, memory_order_relaxed);
        (void)ck_spinlock_lock_eb(&handler->spinlock);
        (void)h2o_linklist_insert(&handler->events, &event->_link);
        (void)ck_spinlock_unlock(&handler->spinlock);
    }
    if (!atomic_flag_test_and_set_explicit(&handler->state, memory_order_relaxed)) {
        (void)create_deferred_action(handler, req, on_ready_input_cb);
        // h2o_nif_handler_data_t *handler_data = h2o_context_get_handler_context(req->conn->ctx, super);
        // ErlNifEnv *env = handler_data->env;
        // ERL_NIF_TERM msg;
        // msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &handler->super), ATOM_ready_input);
        // if (!h2o_nif_port_send(NULL, &handler->super, env, msg)) {
        //     (void)atomic_flag_clear_explicit(&handler->state, memory_order_relaxed);
        // }
        // (void)enif_clear_env(env);
    }

    return 0;
}

static h2o_nif_handler_deferred_action_t *
create_deferred_action(h2o_nif_handler_t *handler, h2o_req_t *req, h2o_timeout_cb cb)
{
    h2o_nif_handler_deferred_action_t *action = h2o_mem_alloc_shared(&req->pool, sizeof(*action), on_deferred_action_dispose);
    *action = (h2o_nif_handler_deferred_action_t){{0, cb}, handler, req};
    (void)h2o_timeout_link(req->conn->ctx->loop, &req->conn->ctx->zero_timeout, &action->timeout);
    return action;
}

static void
on_deferred_action_dispose(void *_action)
{
    h2o_nif_handler_deferred_action_t *action = _action;
    if (h2o_timeout_is_linked(&action->timeout)) {
        (void)h2o_timeout_unlink(&action->timeout);
    }
}

static void
on_ready_input_cb(h2o_timeout_entry_t *entry)
{
    h2o_nif_handler_deferred_action_t *action = H2O_STRUCT_FROM_MEMBER(h2o_nif_handler_deferred_action_t, timeout, entry);
    // if (!h2o_nif_port_send(NULL, &action->handler->super, NULL, ATOM_ready_input)) {
    //     (void)atomic_flag_clear_explicit(&action->handler->state, memory_order_relaxed);
    // }
    h2o_nif_handler_t *handler = action->handler;
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)atomic_load_explicit(&handler->ctx, memory_order_relaxed);
    h2o_req_t *req = action->req;
    h2o_nif_handler_data_t *handler_data = h2o_context_get_handler_context(req->conn->ctx, &ctx->super);
    ErlNifEnv *env = handler_data->env;
    ERL_NIF_TERM msg;
    msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &handler->super), ATOM_ready_input);
    if (!h2o_nif_port_send(NULL, &handler->super, env, msg)) {
        (void)atomic_flag_clear_explicit(&handler->state, memory_order_relaxed);
    }
    (void)enif_clear_env(env);
}
