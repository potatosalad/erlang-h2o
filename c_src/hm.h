// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_HM_H
#define H2O_NIF_HM_H

#include <inttypes.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

typedef struct h2o_nif_hm_stat_s {
    size_t n_buckets;
    size_t size;
} h2o_nif_hm_stat_t;

typedef int h2o_nif_hm_iterator_t(uintptr_t key, uintptr_t value, uintptr_t *acc);

extern int h2o_nif_hm_load(void);
extern void h2o_nif_hm_unload(void);
extern int h2o_nif_hm_get(uintptr_t key, uintptr_t *value, bool keep_resource);
extern int h2o_nif_hm_put(uintptr_t key, uintptr_t value);
extern int h2o_nif_hm_del(uintptr_t key);
extern int h2o_nif_hm_foreach(h2o_nif_hm_iterator_t *iterator, uintptr_t *acc);
extern int h2o_nif_hm_stat(h2o_nif_hm_stat_t *stat);

#endif
