// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_GLOBALS_H
#define H2O_NIF_GLOBALS_H

#include <inttypes.h>
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

#include "khash.h"
#include <h2o.h>
#include <h2o/websocket.h>

#define MAX_PER_SLICE 20000 // 20 KB

extern int erts_fprintf(FILE *, const char *, ...);

#define TRACE 1
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

typedef struct h2o_nif_config_s h2o_nif_config_t;
typedef struct h2o_nif_cfg_listen_s h2o_nif_cfg_listen_t;

typedef struct h2o_nif_handler_s h2o_nif_handler_t;
typedef struct h2o_nif_hdl_child_s h2o_nif_hdl_child_t;
typedef enum h2o_nif_hdl_type_t h2o_nif_hdl_type_t;

typedef struct h2o_nif_port_s h2o_nif_port_t;

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

KHASH_MAP_INIT_INT64(h2o_nif_port_t, h2o_nif_port_t *)

extern ErlNifMutex *h2o_nif_mutex;
extern ErlNifRWLock *h2o_nif_rwlock;
extern h2o_sem_t h2o_ocsp_updater_semaphore;
extern khash_t(h2o_nif_port_t) * h2o_nif_ports;
extern volatile sig_atomic_t h2o_nif_num_ports;
extern volatile sig_atomic_t h2o_nif_seq_ports;

ERL_NIF_TERM ATOM_active;
ERL_NIF_TERM ATOM_already_started;
ERL_NIF_TERM ATOM_badcfg;
ERL_NIF_TERM ATOM_children;
ERL_NIF_TERM ATOM_closed;
ERL_NIF_TERM ATOM_connected;
ERL_NIF_TERM ATOM_error;
ERL_NIF_TERM ATOM_false;
ERL_NIF_TERM ATOM_h2o_port;
ERL_NIF_TERM ATOM_h2o_port_closed;
ERL_NIF_TERM ATOM_h2o_port_data;
ERL_NIF_TERM ATOM_kh_n_buckets;
ERL_NIF_TERM ATOM_kh_size;
ERL_NIF_TERM ATOM_mem_info;
ERL_NIF_TERM ATOM_nil;
ERL_NIF_TERM ATOM_num_ports;
ERL_NIF_TERM ATOM_ok;
ERL_NIF_TERM ATOM_once;
ERL_NIF_TERM ATOM_parent;
ERL_NIF_TERM ATOM_seq_ports;
ERL_NIF_TERM ATOM_true;
ERL_NIF_TERM ATOM_type;
ERL_NIF_TERM ATOM_undefined;

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