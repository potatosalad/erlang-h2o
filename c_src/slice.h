// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_SLICE_H
#define H2O_NIF_SLICE_H

#include "globals.h"

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

typedef size_t(h2o_nif_slice_map_t)(ErlNifEnv *env, h2o_nif_slice_t *slice, size_t offset, size_t length);
typedef ERL_NIF_TERM(h2o_nif_slice_reduce_t)(ErlNifEnv *env, h2o_nif_slice_t *slice);

struct h2o_nif_slice_s {
    const char *fun_name;
    size_t max_per_slice;
    size_t length;
    size_t offset;
    size_t offset2;
    int flags;
    ErlNifBinary in;
    ErlNifBinary out;
    h2o_nif_slice_map_t *map;
    h2o_nif_slice_reduce_t *reduce;
    h2o_mem_pool_t *pool;
    void *data;
};

extern h2o_nif_slice_t *h2o_nif_slice_alloc(ErlNifEnv *env);
extern h2o_nif_slice_t *h2o_nif_slice_create(ErlNifEnv *env, const char *fun_name, size_t length, size_t offset,
                                             h2o_nif_slice_map_t *map, h2o_nif_slice_reduce_t *reduce, void *data);
extern void h2o_nif_slice_release(h2o_nif_slice_t *slice);
extern void h2o_nif_slice_dtor(ErlNifEnv *env, void *obj);
extern ERL_NIF_TERM h2o_nif_slice_schedule(ErlNifEnv *env, h2o_nif_slice_t *slice);
extern ERL_NIF_TERM h2o_nif_slice_mapreduce(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
