// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

static ERL_NIF_TERM
h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 0 || !h2o_nif_server_open(&port)) {
        return enif_make_badarg(env);
    }
    ErlNifPid owner;
    (void)enif_self(env, &owner);
    (void)h2o_nif_port_set_owner(port, owner);
    (void)h2o_nif_port_cas_set_state(port, H2O_NIF_PORT_STATE_OPEN);
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &port, &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    ERL_NIF_TERM out;
    if (!h2o_nif_config_get(env, &server->config, &out)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_release(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    h2o_nif_server_t *server = NULL;
    if (argc != 2 || !h2o_nif_server_get(env, argv[0], &port, &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    ErlNifBinary input;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    if (!h2o_nif_config_set(env, port, &server->config, &input, &out)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_release(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &port, &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    if (!h2o_nif_server_start(port, server)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_release(port);
    return ATOM_ok;
}
