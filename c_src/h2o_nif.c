// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "h2o_nif.h"
#include "server.h"
#include "handler.h"
#include "request.h"

#define H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS 32

#define H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS 10

/*
 * Erlang NIF functions
 */

static ERL_NIF_TERM
h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    h2o_nif_server_t *server = h2o_nif_server_alloc(env);
    if (!h2o_nif_server_init(server)) {
        (void)enif_release_resource((void *)server);
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out = enif_make_resource(env, (void *)server);
    (void)enif_release_resource((void *)server);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *server_type = priv_data->server;
    h2o_nif_server_t *server = NULL;
    if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
        return enif_make_badarg(env);
    }
    return h2o_nif_config_get(env, &server->config);
}

static ERL_NIF_TERM
h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *server_type = priv_data->server;
    h2o_nif_server_t *server = NULL;
    if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary input;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }
    return h2o_nif_config_set(env, &server->config, &input);
}

static ERL_NIF_TERM
h2o_nif_server_getstatus_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *server_type = priv_data->server;
    h2o_nif_server_t *server = NULL;
    if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
        return enif_make_badarg(env);
    }
    switch (server->status) {
    case H2O_NIF_SRV_S_OPEN:
        return enif_make_atom(env, "open");
    case H2O_NIF_SRV_S_IDLE:
        return enif_make_atom(env, "idle");
    case H2O_NIF_SRV_S_LOOP:
        return enif_make_atom(env, "loop");
    default:
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM
h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *server_type = priv_data->server;
    h2o_nif_server_t *server = NULL;
    if (!enif_get_resource(env, argv[0], server_type, (void **)&server)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_server_start(env, server)) {
        return enif_make_badarg(env);
    }
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
h2o_nif_request_reply_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *request_type = priv_data->request;
    h2o_nif_request_t *request = NULL;
    if (!enif_get_resource(env, argv[0], request_type, (void **)&request)) {
        return enif_make_badarg(env);
    }
    h2o_req_t *req = request->req;
    static h2o_generator_t generator = {NULL, NULL};
    req->res.status = 200;
    req->res.reason = "OK";
    h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain; charset=utf-8"));
    h2o_start_response(req, &generator);
    h2o_send(req, &req->entity, 1, 1);
    // h2o_dispose_request(req);
    return enif_make_atom(env, "ok");
}

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
    h2o_nif_data_t *data = enif_alloc(sizeof(*data));
    if (data == NULL) {
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    data->version = h2o_nif_data_version;
    data->server =
        enif_open_resource_type(env, NULL, "h2o_server", h2o_nif_server_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (data->server == NULL) {
        (void)enif_free(data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    data->handler =
        enif_open_resource_type(env, NULL, "h2o_handler", h2o_nif_handler_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (data->handler == NULL) {
        (void)enif_free(data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    data->request =
        enif_open_resource_type(env, NULL, "h2o_request", h2o_nif_request_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (data->request == NULL) {
        (void)enif_free(data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    *priv_data = (void *)(data);
    h2o_srand();
    h2o_hostinfo_max_threads = H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS;
    (void)h2o_sem_init(&h2o_ocsp_updater_semaphore, H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS);
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
    {"server_open", 0, h2o_nif_server_open_0},     {"server_getcfg", 1, h2o_nif_server_getcfg_1},
    {"server_setcfg", 2, h2o_nif_server_setcfg_2}, {"server_getstatus", 1, h2o_nif_server_getstatus_1},
    {"server_start", 1, h2o_nif_server_start_1},   {"request_reply", 1, h2o_nif_request_reply_1},
    // {"is_running", 1, h2o_nif_is_running_1},
};

ERL_NIF_INIT(h2o_nif, h2o_nif_funcs, h2o_nif_load, NULL, h2o_nif_upgrade, h2o_nif_unload);
