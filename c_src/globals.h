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

#include <h2o.h>
#include <h2o/websocket.h>

#include <ck_fifo.h>
#include <ck_pr.h>
#include <ck_spinlock.h>

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

#define MAX_PER_SLICE 20000 // 20 KB

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

/* Global Types */

typedef struct h2o_nif_data_0_s {
    uint8_t version;
} h2o_nif_data_0_t;

#define h2o_nif_data_version 0
#define h2o_nif_data_t h2o_nif_data_0_t

/* Global Variables */

extern ErlNifMutex *h2o_nif_mutex;
extern ErlNifRWLock *h2o_nif_rwlock;
extern h2o_sem_t h2o_ocsp_updater_semaphore;

extern ERL_NIF_TERM ATOM_accept;
extern ERL_NIF_TERM ATOM_active;
extern ERL_NIF_TERM ATOM_already_started;
extern ERL_NIF_TERM ATOM_avg;
extern ERL_NIF_TERM ATOM_badcfg;
extern ERL_NIF_TERM ATOM_children;
extern ERL_NIF_TERM ATOM_closed;
extern ERL_NIF_TERM ATOM_configured;
extern ERL_NIF_TERM ATOM_connected;
extern ERL_NIF_TERM ATOM_eagain;
extern ERL_NIF_TERM ATOM_error;
extern ERL_NIF_TERM ATOM_false;
extern ERL_NIF_TERM ATOM_finalized;
extern ERL_NIF_TERM ATOM_gc_avg;
extern ERL_NIF_TERM ATOM_gc_max;
extern ERL_NIF_TERM ATOM_gc_min;
extern ERL_NIF_TERM ATOM_h2o_handler;
extern ERL_NIF_TERM ATOM_h2o_port;
extern ERL_NIF_TERM ATOM_h2o_port_closed;
extern ERL_NIF_TERM ATOM_h2o_port_data;
extern ERL_NIF_TERM ATOM_hm_stat;
extern ERL_NIF_TERM ATOM_in_progress;
extern ERL_NIF_TERM ATOM_listening;
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
extern ERL_NIF_TERM ATOM_open;
extern ERL_NIF_TERM ATOM_parent;
extern ERL_NIF_TERM ATOM_ports_stat;
extern ERL_NIF_TERM ATOM_ready_input;
extern ERL_NIF_TERM ATOM_seq;
extern ERL_NIF_TERM ATOM_seq_ports;
extern ERL_NIF_TERM ATOM_size;
extern ERL_NIF_TERM ATOM_started;
extern ERL_NIF_TERM ATOM_state;
extern ERL_NIF_TERM ATOM_trap;
extern ERL_NIF_TERM ATOM_true;
extern ERL_NIF_TERM ATOM_type;
extern ERL_NIF_TERM ATOM_undefined;

/* NIF Functions */

extern int h2o_nif_globals_load(ErlNifEnv *env, h2o_nif_data_t *nif_data);
extern int h2o_nif_globals_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
extern void h2o_nif_globals_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data);

#endif