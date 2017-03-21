// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../server.h"

/* fun h2o_nif:server_open/0 */

static ERL_NIF_TERM
h2o_nif_server_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 0 || !h2o_nif_server_open(&server)) {
        return enif_make_badarg(env);
    }
    ErlNifPid owner;
    (void)enif_self(env, &owner);
    (void)h2o_nif_port_set_owner(&server->super, owner);
    assert(h2o_nif_port_set_open(&server->super));
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, &server->super);
    return out;
}

/* fun h2o_nif:server_getcfg/1 */

static ERL_NIF_TERM
h2o_nif_server_getcfg_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    ERL_NIF_TERM out;
    if (!h2o_nif_config_get(&server->config, env, &out)) {
        return enif_make_badarg(env);
    }
    return out;
}

/* fun h2o_nif:server_setcfg/2 */

static ERL_NIF_TERM
h2o_nif_server_setcfg_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 2 || !h2o_nif_server_get(env, argv[0], &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    ErlNifBinary input;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &input)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    if (!h2o_nif_config_set(&server->config, env, &input, &out)) {
        return enif_make_badarg(env);
    }
    return out;
}

/* fun h2o_nif:server_start/1 */

static ERL_NIF_TERM
h2o_nif_server_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_server_t *server = NULL;
    if (argc != 1 || !h2o_nif_server_get(env, argv[0], &server)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (h2o_nif_port_is_started(&server->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    if (!h2o_nif_server_start(server)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_already_started);
    }
    return ATOM_ok;
}
