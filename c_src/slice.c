// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "slice.h"

#ifndef timersub
#define timersub(tvp, uvp, vvp)                                                                                                    \
    do {                                                                                                                           \
        (vvp)->tv_sec = (tvp)->tv_sec - (uvp)->tv_sec;                                                                             \
        (vvp)->tv_usec = (tvp)->tv_usec - (uvp)->tv_usec;                                                                          \
        if ((vvp)->tv_usec < 0) {                                                                                                  \
            (vvp)->tv_sec--;                                                                                                       \
            (vvp)->tv_usec += 1000000;                                                                                             \
        }                                                                                                                          \
    } while ((vvp)->tv_usec >= 1000000)
#endif

static ERL_NIF_TERM
h2o_nif_slice_reduce_binary(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    return enif_make_binary(env, &slice->out);
}

h2o_nif_slice_t *
h2o_nif_slice_alloc(ErlNifEnv *env)
{
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *slice_type = priv_data->slice;
    h2o_nif_slice_t *slice = enif_alloc_resource(slice_type, sizeof(*slice));
    if (slice == NULL) {
        return NULL;
    }
    (void)memset(slice, 0, sizeof(*slice));
    return slice;
}

h2o_nif_slice_t *
h2o_nif_slice_create(ErlNifEnv *env, const char *fun_name, size_t length, size_t offset, h2o_nif_slice_map_t *map,
                     h2o_nif_slice_reduce_t *reduce, void *data)
{
    h2o_nif_slice_t *slice = h2o_nif_slice_alloc(env);
    if (slice == NULL) {
        return NULL;
    }
    slice->fun_name = fun_name;
    slice->max_per_slice = MAX_PER_SLICE;
    slice->length = length;
    slice->offset = offset;
    slice->offset2 = 0;
    slice->flags = 0;
    slice->map = map;
    if (reduce == NULL) {
        reduce = h2o_nif_slice_reduce_binary;
    }
    slice->reduce = reduce;
    slice->pool = NULL;
    slice->data = data;
    return slice;
}

void
h2o_nif_slice_release(h2o_nif_slice_t *slice)
{
    (void)enif_release_resource((void *)slice);
    return;
}

void
h2o_nif_slice_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_slice_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_slice_t *slice = (h2o_nif_slice_t *)obj;
    if (slice->pool != NULL) {
        (void)h2o_mem_clear_pool(slice->pool);
        (void)enif_free(slice->pool);
        slice->pool = NULL;
    }
    if (slice->data != NULL) {
        (void)enif_free(slice->data);
        slice->data = NULL;
    }
    return;
}

ERL_NIF_TERM
h2o_nif_slice_schedule(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM newargv[1];

    newargv[0] = enif_make_resource(env, (void *)slice);

    (void)enif_release_resource((void *)slice);

    return enif_schedule_nif(env, slice->fun_name, 0, h2o_nif_slice_mapreduce, 1, newargv);
}

ERL_NIF_TERM
h2o_nif_slice_mapreduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *slice_type = priv_data->slice;
    h2o_nif_slice_t *slice = NULL;

    if (argc != 1 || !enif_get_resource(env, argv[0], slice_type, (void **)&slice)) {
        return enif_make_badarg(env);
    }

    struct timeval start;
    struct timeval stop;
    struct timeval timeslice;
    size_t end;
    size_t i;
    int percent;
    int total;
    size_t max_per_slice;
    size_t offset;

    total = 0;
    max_per_slice = slice->max_per_slice;
    offset = slice->offset;

    end = offset + max_per_slice;

    if (end > slice->length) {
        end = slice->length;
    }

    i = offset;

    while (i < slice->length) {
        (void)gettimeofday(&start, NULL);
        i = slice->map(env, slice, i, end - i);
        if (i == slice->length) {
            break;
        }
        (void)gettimeofday(&stop, NULL);
        /* determine how much of the timeslice was used */
        timersub(&stop, &start, &timeslice);
        percent = (int)((timeslice.tv_sec * 1000000 + timeslice.tv_usec) / 10);
        total += percent;
        if (percent > 100) {
            percent = 100;
        } else if (percent == 0) {
            percent = 1;
        }
        if (enif_consume_timeslice(env, percent)) {
            /* the timeslice has been used up, so adjust our max_per_slice byte count based on the processing we've done, then
             * reschedule to run again */
            max_per_slice = i - offset;
            if (total > 100) {
                int m = (int)(total / 100);
                if (m == 1) {
                    max_per_slice -= (unsigned long)(max_per_slice * (total - 100) / 100);
                } else {
                    max_per_slice = (unsigned long)(max_per_slice / m);
                }
            }
            slice->max_per_slice = max_per_slice;
            slice->offset = i;
            return enif_schedule_nif(env, slice->fun_name, 0, h2o_nif_slice_mapreduce, argc, argv);
        }
        end += max_per_slice;
        if (end > slice->length) {
            end = slice->length;
        }
    }

    slice->max_per_slice = max_per_slice;
    slice->offset = i;

    return slice->reduce(env, slice);
}
