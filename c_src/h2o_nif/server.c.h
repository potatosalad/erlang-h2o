// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

static ERL_NIF_TERM
h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    h2o_nif_server_t *server = h2o_nif_server_create(env);
    if (server == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, server->port);
    (void)enif_release_resource((void *)server->port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &server)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rlock(server->port)) {
        return enif_make_badarg(env);
    }
    if (server->port->closed) {
        (void)h2o_nif_port_runlock(server->port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM out;
    out = h2o_nif_config_get(env, &server->config);
    (void)h2o_nif_port_runlock(server->port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    ErlNifBinary input;
    if (argc != 2 || !h2o_nif_server_get(env, argv[0], &server) || !enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rwlock(server->port)) {
        return enif_make_badarg(env);
    }
    if (server->port->closed) {
        (void)h2o_nif_port_rwunlock(server->port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM out;
    out = h2o_nif_config_set(env, &server->config, &input);
    (void)h2o_nif_port_rwunlock(server->port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &server)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rwlock(server->port)) {
        return enif_make_badarg(env);
    }
    if (server->port->closed) {
        (void)h2o_nif_port_rwunlock(server->port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!H2O_NIF_PORT_IS_CONFIGURED(server->port)) {
        (void)h2o_nif_port_rwunlock(server->port);
        return enif_make_tuple2(env, ATOM_error, ATOM_badcfg);
    }
    if (H2O_NIF_PORT_IS_STARTED(server->port)) {
        (void)h2o_nif_port_rwunlock(server->port);
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    if (!h2o_nif_server_start(env, server)) {
        (void)h2o_nif_port_rwunlock(server->port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_rwunlock(server->port);
    return ATOM_ok;
}
