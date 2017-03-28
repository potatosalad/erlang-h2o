// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../port.h"

/* port_connect/2 */

static int port_connect_2_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static int port_connect_2_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static h2o_nif_batch_fun_t port_connect_2 = {.test = port_connect_2_test, .exec = port_connect_2_exec, .done = NULL};

static int
port_connect_2_test(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("port_connect_2_test:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = NULL;
    ErlNifPid new_owner;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port) || !enif_get_local_pid(env, argv[1], &new_owner)) {
        return 0;
    }
    return 1;
}

static int
port_connect_2_exec(h2o_nif_batch_ctx_t *ctx, ErlNifEnv *_env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("port_connect_2_exec:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = NULL;
    ErlNifPid new_owner;
    ErlNifEnv *env = ctx->batch->env;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port) || !enif_get_local_pid(env, argv[1], &new_owner)) {
        return 0;
    }
    (void)h2o_nif_port_connect(port, _env, new_owner);
    return 1;
}
