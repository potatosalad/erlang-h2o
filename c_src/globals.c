// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "globals.h"
#include "port.h"

#define H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS 32

#define H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS 10

ErlNifMutex *h2o_nif_mutex = NULL;
ErlNifRWLock *h2o_nif_rwlock = NULL;
h2o_sem_t h2o_ocsp_updater_semaphore;
// khash_t(h2o_nif_port_t) *h2o_nif_ports = NULL;
// volatile sig_atomic_t h2o_nif_num_ports = 0;
// volatile sig_atomic_t h2o_nif_seq_ports = 0;

// /* Hash Table */
// static ck_spinlock_t h2o_nif_ht_spinlock = CK_SPINLOCK_INITIALIZER;
// static ck_ht_t h2o_nif_ht CK_CC_CACHELINE;
// static ck_epoch_t h2o_nif_ht_epoch;
// static ck_epoch_record_t h2o_nif_ht_epoch_wr;
// static void *_h2o_nif_ht_malloc(size_t size);
// static void *_h2o_nif_ht_realloc(void *block, size_t old_size, size_t new_size, bool defer);
// static void _h2o_nif_ht_free(void *block, size_t old_size, bool defer);
// static void _h2o_nif_ht_destroy(ck_epoch_entry_t *e);
// static struct ck_malloc h2o_nif_ht_allocator = {
//     .malloc = _h2o_nif_ht_malloc,
//     .realloc = _h2o_nif_ht_realloc,
//     .free = _h2o_nif_ht_free
// };

// static atomic_bool h2o_nif_globals_state = ATOMIC_VAR_INIT(false);
// static inline int _h2o_nif_globals_state_compare_and_swap(bool expect, bool desired);

// ck_fifo_mpmc_t h2o_nif_fifo CK_CC_CACHELINE;

// static void *h2o_nif_globals_run_loop(void *arg)
// {
//     (void)arg;
//     h2o_nif_fifo_entry_t *entry = NULL;
//     // ck_fifo_mpmc_entry_t *fifo_entry = NULL;
//     ck_fifo_mpmc_entry_t *garbage = NULL;
//     ErlNifEnv *env = enif_alloc_env();
//     // ck_backoff_t backoff = CK_BACKOFF_INITIALIZER;
//     do {
//         while (ck_fifo_mpmc_trydequeue(&h2o_nif_fifo, &entry, &garbage) == false) {
//             ck_pr_stall();
//         }
//         if (garbage != NULL) {
//             (void)free(garbage);
//         }
//         if (entry != NULL) {
//             ERL_NIF_TERM msg = (enif_is_process_alive(env, &entry->pid)) ? ATOM_true : ATOM_false;
//             if (!enif_send(NULL, &entry->pid, env, msg)) {
//                 TRACE_F("unable to send term\n");
//             }
//             (void)enif_clear_env(env);
//             (void)enif_free(entry);
//         }
//     } while (1);
//     return NULL;
// }

int
h2o_nif_globals_load(h2o_nif_data_t *nif_data)
{
    // TRACE_F("about to initialize globals\n");
    // if (!_h2o_nif_globals_state_compare_and_swap(false, true)) {
    //     // TRACE_F("globals already initialized\n");
    //     return 1;
    // }
    // TRACE_F("globals initializing\n");
    h2o_srand();
    h2o_hostinfo_max_threads = H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS;
    (void)h2o_sem_init(&h2o_ocsp_updater_semaphore, H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS);
    // h2o_nif_ports = kh_init(h2o_nif_port_t);
    (void)h2o_nif_hm_load();
    (void)h2o_nif_ports_load(nif_data);
    // /* Hash Table */
    // (void)ck_epoch_init(&h2o_nif_ht_epoch);
    // (void)ck_epoch_register(&h2o_nif_ht_epoch, &h2o_nif_ht_epoch_wr);
    // if (!ck_ht_init(&h2o_nif_ht, CK_HT_MODE_DIRECT | CK_HT_WORKLOAD_DELETE, NULL, &h2o_nif_ht_allocator, 8, 0x0998d9)) {
    //     TRACE_F("error creating global hash table\n");
    // }
    // (void)ck_fifo_mpmc_init(&h2o_nif_fifo, malloc(sizeof(ck_fifo_mpmc_entry_t)));
    // ErlNifTid tid;
    // (void)enif_thread_create("h2o_nif_globals_thread", &tid, h2o_nif_globals_run_loop, NULL, NULL);
    return 1;
    // if (ck_ht_init(&ht, CK_HT_MODE_DIRECT, hash_function, &my_allocator, 8, common_lrand48()) == false) {
    //     perror("ck_ht_init");
    //     exit(EXIT_FAILURE);
    // }
}

void
h2o_nif_globals_unload(void)
{
    // if (!_h2o_nif_globals_state_compare_and_swap(true, false)) {
    //     return 1;
    // }
    h2o_hostinfo_max_threads = 1;
    (void)h2o_sem_destroy(&h2o_ocsp_updater_semaphore);
    (void)h2o_nif_hm_unload();
    return;
}

// /* Hash Table Functions */

// int
// h2o_nif_ht_del(uintptr_t key)
// {
//     ck_ht_entry_t entry;
//     ck_ht_hash_t h;
//     int result;
//     (void)ck_ht_hash_direct(&h, &h2o_nif_ht, key);
//     (void)ck_ht_entry_key_set_direct(&entry, key);
//     (void)h2o_nif_ht_lock();
//     result = ck_ht_remove_spmc(&h2o_nif_ht, h, &entry);
//     (void)h2o_nif_ht_unlock();
//     return result;
// }

// int
// h2o_nif_ht_get(uintptr_t key, uintptr_t *value)
// {
//     ck_ht_entry_t entry;
//     ck_ht_hash_t h;
//     int result;
//     (void)ck_ht_hash_direct(&h, &h2o_nif_ht, key);
//     (void)ck_ht_entry_key_set_direct(&entry, key);
//     result = ck_ht_get_spmc(&h2o_nif_ht, h, &entry);
//     if (value != NULL) {
//         *value = (result) ? ck_ht_entry_value_direct(&entry) : (uintptr_t)NULL;
//     }
//     return result;
// }

// int
// h2o_nif_ht_put(uintptr_t key, uintptr_t value)
// {
//     ck_ht_entry_t entry;
//     ck_ht_hash_t h;
//     int result;
//     (void)ck_ht_hash_direct(&h, &h2o_nif_ht, key);
//     (void)ck_ht_entry_set_direct(&entry, h, key, value);
//     (void)h2o_nif_ht_lock();
//     result = ck_ht_put_spmc(&h2o_nif_ht, h, &entry);
//     (void)h2o_nif_ht_unlock();
//     return result;
// }

// CK_HT_TYPE
// h2o_nif_ht_size(void)
// {
//     return ck_ht_count(&h2o_nif_ht);
// }

// void
// h2o_nif_ht_iterator_init(ck_ht_iterator_t *iterator)
// {
//     (void)ck_ht_iterator_init(iterator);
// }

// bool
// h2o_nif_ht_next(ck_ht_iterator_t *iterator, ck_ht_entry_t **entry)
// {
//     return ck_ht_next(&h2o_nif_ht, iterator, entry);
// }

// void
// h2o_nif_ht_lock(void)
// {
//     (void)ck_spinlock_lock_eb(&h2o_nif_ht_spinlock);
// }

// void
// h2o_nif_ht_unlock(void)
// {
//     (void)ck_spinlock_unlock(&h2o_nif_ht_spinlock);
// }

// void
// h2o_nif_ht_epoch_barrier(void)
// {
//     (void)ck_epoch_barrier(&h2o_nif_ht_epoch_wr);
// }

// /* Internal Functions */

// static void *
// _h2o_nif_ht_malloc(size_t size)
// {
//     TRACE_F("_h2o_nif_ht_malloc\n");
//     ck_epoch_entry_t *e = (ck_epoch_entry_t *)enif_alloc(sizeof(*e) + size);
//     if (e == NULL) {
//         return NULL;
//     }
//     (void)memset(e, 0, size);
//     void *block = (void *)(e + 1);
//     return block;
// }

// static void *
// _h2o_nif_ht_realloc(void *block, size_t old_size, size_t new_size, bool defer)
// {
//     void *new_block = new_block = _h2o_nif_ht_malloc(new_size);
//     if (new_block != NULL) {
//         (void)memcpy(new_block, block, (new_size > old_size) ? old_size : new_size);
//     }
//     (void)_h2o_nif_ht_free(block, old_size, defer);
//     return new_block;
// }

// struct __ck_epoch_entry_s {
//     ck_epoch_entry_t epoch_entry;
// };

// static void
// _h2o_nif_ht_free(void *block, size_t old_size, bool defer)
// {
//     TRACE_F("_h2o_nif_ht_free\n");
//     struct __ck_epoch_entry_s *e = block;
//     (void)old_size;
//     if (defer == true) {
//         /* Destruction requires safe memory reclamation. */
//         (void)ck_epoch_call(&h2o_nif_ht_epoch_wr, &(--e)->epoch_entry, _h2o_nif_ht_destroy);
//     } else {
//         (void)enif_free(--e);
//     }
//     return;
// }

// static void
// _h2o_nif_ht_destroy(ck_epoch_entry_t *e)
// {
//     TRACE_F("_h2o_nif_ht_destroy\n");
//     (void)enif_free(e);
//     return;
// }

// static inline int
// _h2o_nif_globals_state_compare_and_swap(bool expect, bool desired)
// {
//     int result;
//     assert(expect != desired);
//     result = atomic_compare_exchange_weak_explicit(&h2o_nif_globals_state, &expect, desired, memory_order_relaxed,
//     memory_order_relaxed);
//     return result;
// }
