// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_RESOURCE_H
#define H2O_NIF_RESOURCE_H

#include "globals.h"

typedef void h2o_nif_resource_dtor_t(void *block);

typedef struct h2o_nif_resource_s {
    _Atomic int refc;
    h2o_nif_resource_dtor_t *dtor;
} h2o_nif_resource_t;

static void *h2o_nif_resource_alloc(size_t size);
static int h2o_nif_resource_keep(void *block);
static int h2o_nif_resource_release(void *block);

inline void *
h2o_nif_resource_alloc(size_t size)
{
    assert(size >= sizeof(h2o_nif_resource_t));
    h2o_nif_resource_t *resource = enif_alloc(size);
    if (resource == NULL) {
        return NULL;
    }
    (void)memset(resource, 0, size);
    (void)atomic_init(&resource->refc, 1);
    resource->dtor = NULL;
    return (void *)resource;
}

inline int
h2o_nif_resource_keep(void *block)
{
    h2o_nif_resource_t *resource = (h2o_nif_resource_t *)block;
    if (resource == NULL) {
        return 0;
    }
    return (atomic_fetch_add_explicit(&resource->refc, 1, memory_order_relaxed) + 1);
}

inline int
h2o_nif_resource_release(void *block)
{
    h2o_nif_resource_t *resource = (h2o_nif_resource_t *)block;
    int retval;
    if (resource == NULL) {
        return 0;
    }
    if ((retval = (atomic_fetch_sub_explicit(&resource->refc, 1, memory_order_relaxed) - 1)) == 0) {
        if (resource->dtor != NULL) {
            (void)resource->dtor((void *)resource);
        }
        (void)enif_free(resource);
    }
    return retval;
}

#endif