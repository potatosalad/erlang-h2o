// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "h2o_nif.h"
#include "port.h"
#include "server.h"
#include "handler.h"
#include "request.h"
#include "slice.h"

#define H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS 32

#define H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS 10

/*
 * Erlang NIF functions
 */

// static void
// notify_all_threads(h2o_nif_server_t *server)
// {
//     unsigned i;
//     for (i = 0; i != server->config.num_threads; ++i) {
//         (void)h2o_multithread_send_message(&server->threads[i].server_notifications, NULL);
//     }
// }

#include "h2o_nif/port.c.h"
#include "h2o_nif/server.c.h"
#include "h2o_nif/string.c.h"

// static ERL_NIF_TERM
// h2o_nif_atom_check_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     ERL_NIF_TERM atom = enif_make_atom(env, "atom");
//     TRACE_F("argv[0] = %p\n"
//             "atom    = %p\n",
//             argv[0], atom);
//     return enif_make_tuple2(env, argv[0], atom);
// }

// static ERL_NIF_TERM
// h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 0) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_server_t *server = h2o_nif_server_alloc(env);
//     if (!h2o_nif_server_init(server)) {
//         (void)enif_release_resource((void *)server);
//         return enif_make_badarg(env);
//     }
//     ERL_NIF_TERM out = enif_make_resource(env, (void *)server);
//     (void)enif_release_resource((void *)server);
//     return out;
// }

// static ERL_NIF_TERM
// h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *server_type = priv_data->server;
//     h2o_nif_server_t *server = NULL;
//     if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
//         return enif_make_badarg(env);
//     }
//     return h2o_nif_config_get(env, &server->config);
// }

// static ERL_NIF_TERM
// h2o_nif_server_getstatus_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *server_type = priv_data->server;
//     h2o_nif_server_t *server = NULL;
//     if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
//         return enif_make_badarg(env);
//     }
//     switch (server->status) {
//     case H2O_NIF_SRV_S_OPEN:
//         return enif_make_atom(env, "open");
//     case H2O_NIF_SRV_S_IDLE:
//         return enif_make_atom(env, "idle");
//     case H2O_NIF_SRV_S_LOOP:
//         return enif_make_atom(env, "loop");
//     default:
//         return enif_make_badarg(env);
//     }
// }

// static ERL_NIF_TERM
// h2o_nif_handler_accept_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 2) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *handler_type = priv_data->handler;
//     h2o_nif_handler_t *handler = NULL;
//     if (!enif_get_resource(env, argv[0], handler_type, (void **)&handler)) {
//         return enif_make_badarg(env);
//     }
//     return h2o_nif_handler_accept(env, handler, argv[1]);
// }

// static ERL_NIF_TERM
// h2o_nif_request_reply_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     h2o_req_t *req = request->req;
//     static h2o_generator_t generator = {NULL, NULL};
//     req->res.status = 200;
//     req->res.reason = "OK";
//     h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, NULL, H2O_STRLIT("text/plain; charset=utf-8"));
//     h2o_start_response(req, &generator);
//     h2o_send(req, &req->entity, 1, 1);
//     // h2o_dispose_request(req);
//     return enif_make_atom(env, "ok");
// }

// static ERL_NIF_TERM
// h2o_nif_request_is_websocket_handshake_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     h2o_req_t *req = request->req;
//     const char *client_key = NULL;
//     if (h2o_is_websocket_handshake(req, &client_key) != 0 || client_key == NULL) {
//         return enif_make_atom(env, "false");
//     }
//     ERL_NIF_TERM client_key_term;
//     unsigned char *client_key_bin = enif_make_new_binary(env, 24, &client_key_term);
//     (void)memcpy(client_key_bin, client_key, 24);
//     return enif_make_tuple2(env, enif_make_atom(env, "true"), client_key_term);
// }

// static ERL_NIF_TERM
// h2o_nif_request_upgrade_to_websocket_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     if (request->ws != NULL) {
//         return enif_make_badarg(env);
//     }
//     const char *client_key = NULL;
//     if (h2o_is_websocket_handshake(request->req, &client_key) != 0 || client_key == NULL) {
//         return enif_make_atom(env, "false");
//     }
//     // request->wsacc.mutex = enif_mutex_create("h2o_wsacc_mutex");
//     // request->wsmsg.mutex = enif_mutex_create("h2o_wsmsg_mutex");
//     request->ws = h2o_upgrade_to_websocket(request->req, client_key, (void *)request, h2o_nif_request_on_ws_message);
//     // struct wslay_event_msg msgarg = {WSLAY_TEXT_FRAME, "test", 4};
//     // wslay_event_queue_msg(request->ws->ws_ctx, &msgarg);
//     (void)enif_self(env, &request->_wspid);
//     request->wspid = &request->_wspid;
//     (void)notify_all_threads(request->handler->server);
//     return enif_make_atom(env, "ok");
// }

// static ERL_NIF_TERM
// h2o_nif_request_delegate_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     if (request->ws != NULL) {
//         return enif_make_badarg(env);
//     }
//     (void)h2o_delegate_request_deferred(request->req, &request->handler->child->super);
//     (void)notify_all_threads(request->handler->server);
//     return enif_make_atom(env, "ok");
// }

// static ERL_NIF_TERM
// h2o_nif_request_send_inline_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 2) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary body_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[1], &body_bin)) {
//         return enif_make_badarg(env);
//     }
//     (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
//     (void)notify_all_threads(request->handler->server);
//     return enif_make_atom(env, "ok");
// }

// static ERL_NIF_TERM
// h2o_nif_request_set_status_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 2) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     unsigned int status = 0;
//     if (!enif_get_uint(env, argv[1], &status) || status < 100 || status > 599) {
//         return enif_make_badarg(env);
//     }
//     request->req->res.status = status;
//     return enif_make_atom(env, "ok");
// }

// static ERL_NIF_TERM
// h2o_nif_request_add_header_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     if (argc != 3) {
//         return enif_make_badarg(env);
//     }
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *request_type = priv_data->request;
//     h2o_nif_request_t *request = NULL;
//     if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary name_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[1], &name_bin)) {
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary value_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[2], &value_bin)) {
//         return enif_make_badarg(env);
//     }
//     (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1,
//                                 NULL, (const char *)value_bin.data, value_bin.size);
//     return enif_make_atom(env, "ok");
// }

/*
 * Erlang NIF callbacks
 */
static int
h2o_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (h2o_nif_mutex == NULL) {
        h2o_nif_mutex = enif_mutex_create("h2o_nif_mutex");
    }
    (void)enif_mutex_lock(h2o_nif_mutex);
    if (h2o_nif_rwlock == NULL) {
        h2o_nif_rwlock = enif_rwlock_create("h2o_nif_rwlock");
    }

/* Initialize common atoms */
#define ATOM(Id, Value)                                                                                                            \
    {                                                                                                                              \
        Id = enif_make_atom(env, Value);                                                                                           \
    }
    ATOM(ATOM_active, "active");
    ATOM(ATOM_already_started, "already_started");
    ATOM(ATOM_badcfg, "badcfg");
    ATOM(ATOM_children, "children");
    ATOM(ATOM_closed, "closed");
    ATOM(ATOM_connected, "connected");
    ATOM(ATOM_error, "error");
    ATOM(ATOM_false, "false");
    ATOM(ATOM_h2o_port, "h2o_port");
    ATOM(ATOM_h2o_port_closed, "h2o_port_closed");
    ATOM(ATOM_h2o_port_data, "h2o_port_data");
    ATOM(ATOM_kh_n_buckets, "kh_n_buckets");
    ATOM(ATOM_kh_size, "kh_size");
    ATOM(ATOM_mem_info, "mem_info");
    ATOM(ATOM_nil, "nil");
    ATOM(ATOM_num_ports, "num_ports");
    ATOM(ATOM_ok, "ok");
    ATOM(ATOM_once, "once");
    ATOM(ATOM_parent, "parent");
    ATOM(ATOM_seq_ports, "seq_ports");
    ATOM(ATOM_true, "true");
    ATOM(ATOM_type, "type");
    ATOM(ATOM_undefined, "undefined");
#undef ATOM

    h2o_nif_data_t *data = enif_alloc(sizeof(*data));
    if (data == NULL) {
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    data->version = h2o_nif_data_version;
    data->slice =
        enif_open_resource_type(env, NULL, "h2o_slice", h2o_nif_slice_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (data->slice == NULL) {
        (void)enif_free(data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    data->port = enif_open_resource_type(env, NULL, "h2o_port", h2o_nif_port_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (data->port == NULL) {
        (void)enif_free(data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    // data->server =
    //     enif_open_resource_type(env, NULL, "h2o_server", h2o_nif_server_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // if (data->server == NULL) {
    //     (void)enif_free(data);
    //     (void)enif_mutex_unlock(h2o_nif_mutex);
    //     return -1;
    // }
    // data->handler =
    //     enif_open_resource_type(env, NULL, "h2o_handler", h2o_nif_handler_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // if (data->handler == NULL) {
    //     (void)enif_free(data);
    //     (void)enif_mutex_unlock(h2o_nif_mutex);
    //     return -1;
    // }
    // data->request =
    //     enif_open_resource_type(env, NULL, "h2o_request", h2o_nif_request_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    // if (data->request == NULL) {
    //     (void)enif_free(data);
    //     (void)enif_mutex_unlock(h2o_nif_mutex);
    //     return -1;
    // }
    *priv_data = (void *)(data);
    h2o_srand();
    h2o_hostinfo_max_threads = H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS;
    (void)h2o_sem_init(&h2o_ocsp_updater_semaphore, H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS);
    h2o_nif_ports = kh_init(h2o_nif_port_t);
    (void)enif_mutex_unlock(h2o_nif_mutex);
    return 0;
}

static int
h2o_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static void
h2o_nif_unload(ErlNifEnv *env, void *priv_data)
{
    if (h2o_nif_mutex != NULL) {
        (void)enif_mutex_lock(h2o_nif_mutex);
    }
    h2o_hostinfo_max_threads = 1;
    (void)h2o_sem_destroy(&h2o_ocsp_updater_semaphore);
    if (priv_data != NULL) {
        (void)enif_free(priv_data);
        priv_data = NULL;
    }
    if (h2o_nif_mutex != NULL) {
        (void)enif_mutex_unlock(h2o_nif_mutex);
        (void)enif_mutex_destroy(h2o_nif_mutex);
        h2o_nif_mutex = NULL;
    }
}

static ErlNifFunc h2o_nif_funcs[] = {
    {"port_open", 0, h2o_nif_port_open_0},
    {"port_open", 1, h2o_nif_port_open_1},
    {"port_close", 1, h2o_nif_port_close_1},
    {"port_connect", 2, h2o_nif_port_connect_2},
    {"port_info", 0, h2o_nif_port_info_0},
    {"port_info", 1, h2o_nif_port_info_1},
    {"port_info", 2, h2o_nif_port_info_2},
    {"port_getopt", 2, h2o_nif_port_getopt_2},
    {"port_setopt", 3, h2o_nif_port_setopt_3},
    {"server_open", 0, h2o_nif_server_open_0},
    {"server_getcfg", 1, h2o_nif_server_getcfg_1},
    {"server_setcfg", 2, h2o_nif_server_setcfg_2},
    {"server_start", 1, h2o_nif_server_start_1},
    {"string_tolower", 1, h2o_nif_string_tolower_1},
    {"string_strtolower", 1, h2o_nif_string_strtolower_1},
    {"string_toupper", 1, h2o_nif_string_toupper_1},
    {"string_strtoupper", 1, h2o_nif_string_strtoupper_1},
    {"string_lcstris", 2, h2o_nif_string_lcstris_2},
    {"string_base64_encode_capacity", 1, h2o_nif_string_base64_encode_capacity_1},
    {"string_decode_base64url", 1, h2o_nif_string_decode_base64url_1},
    {"string_base64_encode", 2, h2o_nif_string_base64_encode_2},
    {"string_hex_decode", 1, h2o_nif_string_hex_decode_1},
    {"string_hex_encode", 1, h2o_nif_string_hex_encode_1},
    {"string_uri_escape", 2, h2o_nif_string_uri_escape_2},
    {"string_get_filext", 1, h2o_nif_string_get_filext_1},
    {"string_str_stripws", 1, h2o_nif_string_str_stripws_1},
    {"string_htmlescape", 1, h2o_nif_string_htmlescape_1},
    // {"atom_check", 1, h2o_nif_atom_check_1},
    // {"server_getstatus", 1, h2o_nif_server_getstatus_1},
    // {"handler_accept", 2, h2o_nif_handler_accept_2},
    // {"request_reply", 1, h2o_nif_request_reply_1},
    // {"request_delegate", 1, h2o_nif_request_delegate_1},
    // {"request_is_websocket_handshake", 1, h2o_nif_request_is_websocket_handshake_1},
    // {"request_upgrade_to_websocket", 1, h2o_nif_request_upgrade_to_websocket_1},
    // {"request_send_inline", 2, h2o_nif_request_send_inline_2},
    // {"request_set_status", 2, h2o_nif_request_set_status_2},
    // {"request_add_header", 3, h2o_nif_request_add_header_3},
    // {"is_running", 1, h2o_nif_is_running_1},
};

ERL_NIF_INIT(h2o_nif, h2o_nif_funcs, h2o_nif_load, NULL, h2o_nif_upgrade, h2o_nif_unload);
