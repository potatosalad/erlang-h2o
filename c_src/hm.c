// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "hm.h"

#ifndef kcalloc
#define kcalloc(N, Z) memset(enif_alloc((N) * (Z)), 0, (N) * (Z))
#endif
#ifndef kmalloc
#define kmalloc(Z) enif_alloc(Z)
#endif
#ifndef krealloc
#define krealloc(P, Z) enif_realloc(P, Z)
#endif
#ifndef kfree
#define kfree(P) enif_free(P)
#endif

#include <khash.h>
#include <ck_spinlock.h>
#include <stdatomic.h>

/* HashMap Structure */

KHASH_MAP_INIT_INT64(uintptr_t, uintptr_t)

typedef struct hm_s {
    int aba;
    khash_t(uintptr_t) * map;
} hm_t;

/* Static Variables */

static ck_spinlock_t hm_spinlock = CK_SPINLOCK_INITIALIZER;
static void hm_lock(void);
static void hm_unlock(void);
static _Atomic hm_t hm_state = ATOMIC_VAR_INIT(((hm_t){0, NULL}));

/* Public Functions */

int
h2o_nif_hm_load(void)
{
    hm_t expected;
    hm_t desired;
    khash_t(uintptr_t) *map = NULL;

    (void)hm_lock();
    expected = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (expected.map != NULL) {
        // already initialized
        (void)hm_unlock();
        return 1;
    }
    map = kh_init(uintptr_t);
    if (map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    desired = (hm_t){.aba = expected.aba + 1, .map = map};
    if (!atomic_compare_exchange_weak_explicit(&hm_state, &expected, desired, memory_order_relaxed, memory_order_relaxed)) {
        // already initialized
        (void)kfree(map);
    }
    (void)hm_unlock();
    return 1;
}

void
h2o_nif_hm_unload(void)
{
    hm_t expected;
    hm_t desired;
    khash_t(uintptr_t) *map = NULL;

    (void)hm_lock();
    expected = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (expected.map == NULL) {
        // already disposed
        (void)hm_unlock();
        return;
    }
    map = expected.map;
    desired = (hm_t){.aba = expected.aba + 1, .map = NULL};
    if (atomic_compare_exchange_weak_explicit(&hm_state, &expected, desired, memory_order_relaxed, memory_order_relaxed)) {
        (void)kh_clear(uintptr_t, map);
        (void)kh_destroy(uintptr_t, map);
        map = NULL;
    }
    (void)hm_unlock();
    return;
}

int
h2o_nif_hm_get(uintptr_t key, uintptr_t *value, bool keep_resource)
{
    hm_t hm;
    khiter_t iter;

    (void)hm_lock();
    if (value != NULL) {
        *value = (uintptr_t)NULL;
    }
    hm = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (hm.map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    iter = kh_get(uintptr_t, hm.map, key);
    if (iter == kh_end(hm.map)) {
        (void)hm_unlock();
        return 0;
    }
    if (value != NULL) {
        if (keep_resource) {
            (void)enif_keep_resource((void *)(kh_val(hm.map, iter)));
        }
        *value = kh_val(hm.map, iter);
    }
    (void)hm_unlock();
    return 1;
}

int
h2o_nif_hm_put(uintptr_t key, uintptr_t value)
{
    hm_t hm;
    khiter_t iter;
    int r;

    (void)hm_lock();
    hm = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (hm.map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    iter = kh_put(uintptr_t, hm.map, key, &r);
    switch (r) {
    case 1: // the bucket is empty (never used)
    case 2: // the element in the bucket has been deleted
        kh_val(hm.map, iter) = value;
        (void)hm_unlock();
        return 1;
    default:
        (void)hm_unlock();
        return r;
    }
}

int
h2o_nif_hm_del(uintptr_t key)
{
    hm_t hm;
    khiter_t iter;

    (void)hm_lock();
    hm = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (hm.map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    iter = kh_get(uintptr_t, hm.map, key);
    if (iter == kh_end(hm.map)) {
        (void)hm_unlock();
        return 0;
    }
    kh_del(uintptr_t, hm.map, iter);
    (void)hm_unlock();
    return 1;
}

int
h2o_nif_hm_foreach(h2o_nif_hm_iterator_t *iterator, uintptr_t *acc)
{
    hm_t hm;
    uintptr_t key;
    uintptr_t value;
    int r = 1;

    if (iterator == NULL) {
        return 0;
    }
    (void)hm_lock();
    hm = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (hm.map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    kh_foreach(hm.map, key, value, {
        r = iterator(key, value, acc);
        if (!r) {
            break;
        }
    })(void) hm_unlock();
    return r;
}

int
h2o_nif_hm_stat(h2o_nif_hm_stat_t *stat)
{
    hm_t hm;

    (void)hm_lock();
    hm = atomic_load_explicit(&hm_state, memory_order_relaxed);
    if (hm.map == NULL) {
        (void)hm_unlock();
        return 0;
    }
    if (stat != NULL) {
        stat->n_buckets = kh_n_buckets(hm.map);
        stat->size = kh_size(hm.map);
    }
    (void)hm_unlock();
    return 1;
}

/* Private Functions */

static inline void
hm_lock(void)
{
    (void)ck_spinlock_lock_eb(&hm_spinlock);
}

static inline void
hm_unlock(void)
{
    (void)ck_spinlock_unlock(&hm_spinlock);
}
