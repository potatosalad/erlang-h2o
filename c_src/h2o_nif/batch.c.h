// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../batch.h"

/* fun h2o_nif:batch/1 */

static ERL_NIF_TERM h2o_nif_batch_continue_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_batch_finalize_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_batch_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_batch_1:%s:%d\n", __FILE__, __LINE__);
    if (argc != 1 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }
    h2o_nif_batch_t *batch = NULL;
    if (!h2o_nif_batch_create(env, argv[0], &batch)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list;
    ERL_NIF_TERM out;
    list = argv[0];
    out = h2o_nif_batch_process(batch, env, &list);
    if (out == ATOM_continue) {
        ERL_NIF_TERM newargv[2];
        newargv[0] = h2o_nif_batch_make(env, batch); /* [0] batch */
        newargv[1] = list;                           /* [1] list */
        (void)h2o_nif_batch_release(batch);
        return enif_schedule_nif(env, "batch", 0, h2o_nif_batch_continue_2, 2, newargv);
    } else if (out == ATOM_finalize) {
        ERL_NIF_TERM newargv[1];
        newargv[0] = h2o_nif_batch_make(env, batch); /* [0] batch */
        (void)h2o_nif_batch_release(batch);
        return enif_schedule_nif(env, "batch", 0, h2o_nif_batch_finalize_1, 1, newargv);
    } else {
        (void)h2o_nif_batch_release(batch);
        return out;
    }
}

static ERL_NIF_TERM
h2o_nif_batch_continue_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_batch_continue_2:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_batch_t *batch = NULL;
    if (argc != 2 || !h2o_nif_batch_get(env, argv[0], &batch) || !enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list;
    ERL_NIF_TERM out;
    list = argv[1];
    out = h2o_nif_batch_process(batch, env, &list);
    if (out == ATOM_continue) {
        ERL_NIF_TERM newargv[2];
        newargv[0] = argv[0]; /* [0] batch */
        newargv[1] = list;    /* [1] list */
        return enif_schedule_nif(env, "batch", 0, h2o_nif_batch_continue_2, 2, newargv);
    } else if (out == ATOM_finalize) {
        ERL_NIF_TERM newargv[1];
        newargv[0] = argv[0]; /* [0] batch */
        return enif_schedule_nif(env, "batch", 0, h2o_nif_batch_finalize_1, 1, newargv);
    } else {
        return out;
    }
}

static ERL_NIF_TERM
h2o_nif_batch_finalize_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_batch_finalize_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_batch_t *batch = NULL;
    if (argc != 1 || !h2o_nif_batch_get(env, argv[0], &batch)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    out = h2o_nif_batch_resolve(batch, env);
    if (out == ATOM_finalize) {
        return enif_schedule_nif(env, "batch", 0, h2o_nif_batch_finalize_1, argc, argv);
    } else {
        return out;
    }
}
