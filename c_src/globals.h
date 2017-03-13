// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_GLOBALS_H
#define H2O_NIF_GLOBALS_H

#include <inttypes.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <erl_nif.h>

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

// #include "khash.h"
#include <h2o.h>
#include <h2o/websocket.h>

#include <ck_fifo.h>
#include <ck_pr.h>
#include <ck_spinlock.h>

#include "hm.h"

#define MAX_PER_SLICE 20000 // 20 KB

extern int erts_fprintf(FILE *, const char *, ...);

// #define TRACE 1
#ifdef TRACE
#define TRACE_C(c)                                                                                                                 \
    do {                                                                                                                           \
        putchar(c);                                                                                                                \
        fflush(stdout);                                                                                                            \
    } while (0)
#define TRACE_S(s)                                                                                                                 \
    do {                                                                                                                           \
        fputs((s), stdout);                                                                                                        \
        fflush(stdout);                                                                                                            \
    } while (0)
#define TRACE_F(...)                                                                                                               \
    do {                                                                                                                           \
        erts_fprintf(stderr, __VA_ARGS__);                                                                                         \
        fflush(stderr);                                                                                                            \
    } while (0)
#else
#define TRACE_C(c) ((void)(0))
#define TRACE_S(s) ((void)(0))
#define TRACE_F(...) ((void)(0))
#endif

#define DEBUG_F(...)                                                                                                               \
    do {                                                                                                                           \
        erts_fprintf(stderr, __VA_ARGS__);                                                                                         \
        fflush(stderr);                                                                                                            \
    } while (0)

/* Global Types */

typedef struct h2o_nif_data_0_s {
    uint8_t version;
    ErlNifResourceType *slice;
    ErlNifResourceType *port;
    ErlNifResourceType *server;
    ErlNifResourceType *handler;
    ErlNifResourceType *request;
} h2o_nif_data_0_t;

#define h2o_nif_data_version 0
#define h2o_nif_data_t h2o_nif_data_0_t

typedef int H2O_NIF_RETURN;
#define H2O_NIF_RETURN_TRAP 1
#define H2O_NIF_RETURN_DONE 2

#define H2O_NIF_MSG_TERM 1

typedef struct h2o_nif_msg_s h2o_nif_msg_t;
struct h2o_nif_msg_s {
    int type;
};

// typedef struct h2o_nif_msg_acc_s h2o_nif_msg_acc_t;
// struct h2o_nif_msg_acc_s {
//     h2o_nif_msg_t super;
//     unsigned long id;
//     ErlNifPid caller;
// };

// typedef struct h2o_nif_msg_req_s h2o_nif_msg_req_t;
// struct h2o_nif_msg_req_s {
//     h2o_nif_msg_t super;
//     h2o_req_t *req;
// };

typedef struct h2o_nif_msg_term_s h2o_nif_msg_term_t;
struct h2o_nif_msg_term_s {
    h2o_nif_msg_t super;
    ErlNifEnv *env;
    ERL_NIF_TERM term;
};

typedef struct h2o_nif_port_s h2o_nif_port_t;

typedef struct h2o_nif_return_ctx_s h2o_nif_return_ctx_t;

typedef H2O_NIF_RETURN h2o_nif_callback_t(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx);

struct h2o_nif_return_ctx_s {
    int state;
    h2o_nif_callback_t *callback;
    void *data;
    ERL_NIF_TERM out;
};

typedef struct h2o_nif_config_s h2o_nif_config_t;
typedef struct h2o_nif_cfg_listen_s h2o_nif_cfg_listen_t;

typedef struct h2o_nif_filter_s h2o_nif_filter_t;
typedef struct h2o_nif_filter_ctx_s h2o_nif_filter_ctx_t;

typedef struct h2o_nif_handler_s h2o_nif_handler_t;
typedef struct h2o_nif_handler_ctx_s h2o_nif_handler_ctx_t;
typedef enum h2o_nif_handler_type_t h2o_nif_handler_type_t;

typedef struct h2o_nif_logger_s h2o_nif_logger_t;
typedef struct h2o_nif_logger_ctx_s h2o_nif_logger_ctx_t;

typedef struct h2o_nif_server_s h2o_nif_server_t;
typedef struct h2o_nif_srv_listen_s h2o_nif_srv_listen_t;
typedef struct h2o_nif_srv_state_s h2o_nif_srv_state_t;
typedef struct h2o_nif_srv_thread_ctx_s h2o_nif_srv_thread_ctx_t;
typedef struct h2o_nif_srv_thread_s h2o_nif_srv_thread_t;

typedef struct h2o_nif_slice_s h2o_nif_slice_t;

// typedef struct h2o_drv_server_s h2o_drv_server_t;
// typedef struct h2o_drv_srv_listen_s h2o_drv_srv_listen_t;
// typedef enum h2o_drv_srv_mode_t h2o_drv_srv_mode_t;
// typedef struct h2o_drv_srv_state_s h2o_drv_srv_state_t;
// typedef struct h2o_drv_srv_thread_s h2o_drv_srv_thread_t;

typedef struct h2o_nif_request_s h2o_nif_request_t;

typedef struct h2o_nif_queue_s h2o_nif_queue_t;
struct h2o_nif_queue_s {
    ErlNifMutex *mutex;
    h2o_linklist_t pending;
};

typedef struct h2o_nif_accept_s h2o_nif_accept_t;
struct h2o_nif_accept_s {
    h2o_linklist_t _link;
    int id;
    ErlNifPid caller;
};

/* Global Variables */

extern ErlNifMutex *h2o_nif_mutex;
extern ErlNifRWLock *h2o_nif_rwlock;
extern h2o_sem_t h2o_ocsp_updater_semaphore;
// extern khash_t(h2o_nif_port_t) * h2o_nif_ports;
// extern volatile sig_atomic_t h2o_nif_num_ports;
// extern volatile sig_atomic_t h2o_nif_seq_ports;

// typedef struct h2o_nif_fifo_entry_s {
//     ErlNifPid pid;
// } h2o_nif_fifo_entry_t;

// extern ck_fifo_mpmc_t h2o_nif_fifo CK_CC_CACHELINE;

extern int h2o_nif_globals_load(h2o_nif_data_t *nif_data);
extern void h2o_nif_globals_unload(void);

// extern int h2o_nif_ht_del(uintptr_t key);
// extern int h2o_nif_ht_get(uintptr_t key, uintptr_t *value);
// extern int h2o_nif_ht_put(uintptr_t key, uintptr_t value);
// extern CK_HT_TYPE h2o_nif_ht_size(void);
// extern void h2o_nif_ht_iterator_init(ck_ht_iterator_t *iterator);
// extern bool h2o_nif_ht_next(ck_ht_iterator_t *iterator, ck_ht_entry_t **entry);
// extern void h2o_nif_ht_lock(void);
// extern void h2o_nif_ht_unlock(void);
// // extern void h2o_nif_ht_epoch_register(ck_epoch_record_t *record);
// // extern void h2o_nif_ht_epoch_unregister(ck_epoch_record_t *record);
// // extern void h2o_nif_ht_epoch_begin(ck_epoch_record_t *record, ck_epoch_section_t *section);
// // extern void h2o_nif_ht_epoch_end(ck_epoch_record_t *record, ck_epoch_section_t *section);
// // extern void h2o_nif_ht_epoch_poll(void);
// extern void h2o_nif_ht_epoch_barrier(void);

// extern ck_spinlock_t h2o_nif_globals_spinlock;
// extern ck_ht_t h2o_nif_globals_ht CK_CC_CACHELINE;
// extern ck_epoch_t h2o_nif_globals_ht_epoch;
// extern ck_epoch_record_t h2o_nif_globals_ht_epoch_wr;

extern ERL_NIF_TERM ATOM_accept;
extern ERL_NIF_TERM ATOM_active;
extern ERL_NIF_TERM ATOM_already_started;
extern ERL_NIF_TERM ATOM_avg;
extern ERL_NIF_TERM ATOM_badcfg;
extern ERL_NIF_TERM ATOM_children;
extern ERL_NIF_TERM ATOM_closed;
extern ERL_NIF_TERM ATOM_connected;
extern ERL_NIF_TERM ATOM_eagain;
extern ERL_NIF_TERM ATOM_error;
extern ERL_NIF_TERM ATOM_false;
extern ERL_NIF_TERM ATOM_gc_avg;
extern ERL_NIF_TERM ATOM_gc_max;
extern ERL_NIF_TERM ATOM_gc_min;
extern ERL_NIF_TERM ATOM_h2o_handler;
extern ERL_NIF_TERM ATOM_h2o_port;
extern ERL_NIF_TERM ATOM_h2o_port_closed;
extern ERL_NIF_TERM ATOM_h2o_port_data;
extern ERL_NIF_TERM ATOM_hm_stat;
extern ERL_NIF_TERM ATOM_max;
extern ERL_NIF_TERM ATOM_mem_info;
extern ERL_NIF_TERM ATOM_min;
extern ERL_NIF_TERM ATOM_n_buckets;
extern ERL_NIF_TERM ATOM_nil;
extern ERL_NIF_TERM ATOM_num_accept;
extern ERL_NIF_TERM ATOM_num_children;
extern ERL_NIF_TERM ATOM_num_listen;
extern ERL_NIF_TERM ATOM_num_output;
extern ERL_NIF_TERM ATOM_num_ports;
extern ERL_NIF_TERM ATOM_ok;
extern ERL_NIF_TERM ATOM_once;
extern ERL_NIF_TERM ATOM_parent;
extern ERL_NIF_TERM ATOM_ports_stat;
extern ERL_NIF_TERM ATOM_seq;
extern ERL_NIF_TERM ATOM_seq_ports;
extern ERL_NIF_TERM ATOM_size;
extern ERL_NIF_TERM ATOM_state;
extern ERL_NIF_TERM ATOM_true;
extern ERL_NIF_TERM ATOM_type;
extern ERL_NIF_TERM ATOM_undefined;

// /* Global Variables */
// typedef struct h2o_drv_s {
//     ErlDrvTermData am_ok;
//     ErlDrvTermData am_error;
//     ErlDrvTermData am_nil;
//     ErlDrvTermData am_undefined;
//     ErlDrvTermData am_h2o_handler;
// } h2o_drv_t;

// extern h2o_drv_t *h2o_drv;
// extern ErlDrvMutex *h2o_mutex;
// extern h2o_sem_t h2o_ocsp_updater_semaphore;

// #define H2O_DRV_ATOM(NAME) (ErlDrvTermData)(h2o_drv->am_##NAME)
// #define H2O_DRV_STRING(NAME) (char *)(h2o_drv->str_##NAME)

// #define H2O_FAIL_BADSPEC(PORT) (void)(driver_failure_atom(PORT, "bad_spec"))
// #define H2O_FAIL_OOM(PORT) (void)(driver_failure_atom(PORT, "out_of_memory"))

// /* Global Types */

// typedef struct h2o_drv_config_s h2o_drv_config_t;
// typedef struct h2o_drv_cfg_listen_s h2o_drv_cfg_listen_t;

// typedef struct h2o_drv_handler_s h2o_drv_handler_t;
// typedef struct h2o_drv_hdl_child_s h2o_drv_hdl_child_t;

// typedef struct h2o_drv_server_s h2o_drv_server_t;
// typedef struct h2o_drv_srv_listen_s h2o_drv_srv_listen_t;
// typedef enum h2o_drv_srv_mode_t h2o_drv_srv_mode_t;
// typedef struct h2o_drv_srv_state_s h2o_drv_srv_state_t;
// typedef struct h2o_drv_srv_thread_s h2o_drv_srv_thread_t;

// typedef struct h2o_drv_request_s h2o_drv_request_t;

#endif