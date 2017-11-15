// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "slice.h"

ErlNifResourceType *h2o_nif_slice_resource_type = NULL;
ErlNifResourceType *h2o_nif_trap_resource_type = NULL;

/* Static Functions (Declarations) */

static h2o_nif_slice_t *h2o_nif_slice_alloc(size_t size);
static ERL_NIF_TERM h2o_nif_slice_mapreduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM h2o_nif_slice_reduce_binary(ErlNifEnv *env, h2o_nif_slice_t *slice);

/* NIF Functions */

int
h2o_nif_slice_load(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    h2o_nif_slice_resource_type =
        enif_open_resource_type(env, NULL, "h2o_nif_slice", h2o_nif_slice_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    h2o_nif_trap_resource_type =
        enif_open_resource_type(env, NULL, "h2o_nif_trap", NULL, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return 0;
}

int
h2o_nif_slice_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

void
h2o_nif_slice_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    return;
}

/* Functions */

int
__h2o_nif_slice_create(size_t size, const char *fun_name, h2o_nif_slice_map_t *map, h2o_nif_slice_reduce_t *reduce,
                       h2o_nif_slice_t **slicep)
{
    assert(slicep != NULL);
    h2o_nif_slice_t *slice = h2o_nif_slice_alloc(size);
    if (slice == NULL) {
        *slicep = NULL;
        return 0;
    }
    (void)strncpy((char *)slice->fun_name, fun_name, sizeof(slice->fun_name) - 1);
    slice->max_per_slice = MAX_PER_SLICE;
    slice->badarg = 0;
    slice->flags = 0;
    (void)h2o_mem_init_pool(&slice->pool);
    slice->in.length = 0;
    slice->in.offset = 0;
    slice->out.length = 0;
    slice->out.offset = 0;
    slice->map = map;
    slice->reduce = (reduce == NULL) ? h2o_nif_slice_reduce_binary : reduce;
    // slice->pool = NULL;
    // slice->data = data;
    *slicep = slice;
    return 1;
}

void
h2o_nif_slice_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_slice_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_slice_t *slice = (h2o_nif_slice_t *)obj;
    (void)h2o_mem_clear_pool(&slice->pool);
    // if (slice->pool != NULL) {
    //     (void)h2o_mem_clear_pool(slice->pool);
    //     (void)enif_free(slice->pool);
    //     slice->pool = NULL;
    // }
    // if (slice->data != NULL) {
    //     (void)enif_free(slice->data);
    //     slice->data = NULL;
    // }
    return;
}

ERL_NIF_TERM
h2o_nif_slice_schedule(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    ERL_NIF_TERM newargv[1];
    newargv[0] = enif_make_resource(env, (void *)slice);
    (void)h2o_nif_slice_release(slice);
    return enif_schedule_nif(env, slice->fun_name, 0, h2o_nif_slice_mapreduce, 1, newargv);
}

/* Static Functions (Definitions) */

static h2o_nif_slice_t *
h2o_nif_slice_alloc(size_t size)
{
    assert(size >= sizeof(h2o_nif_slice_t));
    h2o_nif_slice_t *slice = (h2o_nif_slice_t *)enif_alloc_resource(h2o_nif_slice_resource_type, size);
    if (slice == NULL) {
        return NULL;
    }
    (void)memset(slice, 0, size);
    return slice;
}

static ERL_NIF_TERM
h2o_nif_slice_mapreduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_slice_t *slice = NULL;

    if (argc != 1 || !enif_get_resource(env, argv[0], h2o_nif_slice_resource_type, (void **)&slice)) {
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
    offset = slice->in.offset;

    end = offset + max_per_slice;

    if (end > slice->in.length) {
        end = slice->in.length;
    }

    i = offset;

    while (i < slice->in.length) {
        (void)gettimeofday(&start, NULL);
        i = slice->map(env, slice, i, end - i);
        if (slice->badarg) {
            return enif_make_badarg(env);
        }
        if (i == slice->in.length) {
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
            slice->in.offset = i;
            return enif_schedule_nif(env, slice->fun_name, 0, h2o_nif_slice_mapreduce, argc, argv);
        }
        end += max_per_slice;
        if (end > slice->in.length) {
            end = slice->in.length;
        }
    }

    slice->max_per_slice = max_per_slice;
    slice->in.offset = i;

    return slice->reduce(env, slice);
}

static ERL_NIF_TERM
h2o_nif_slice_reduce_binary(ErlNifEnv *env, h2o_nif_slice_t *slice)
{
    return enif_make_binary(env, &slice->out.binary);
}
