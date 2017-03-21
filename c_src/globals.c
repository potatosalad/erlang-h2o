// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "globals.h"
#include "port.h"
#include "slice.h"

#define H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS 32

#define H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS 10

/* Global Variables */

ErlNifMutex *h2o_nif_mutex = NULL;
ErlNifRWLock *h2o_nif_rwlock = NULL;
h2o_sem_t h2o_ocsp_updater_semaphore;

ERL_NIF_TERM ATOM_accept;
ERL_NIF_TERM ATOM_active;
ERL_NIF_TERM ATOM_already_started;
ERL_NIF_TERM ATOM_avg;
ERL_NIF_TERM ATOM_badcfg;
ERL_NIF_TERM ATOM_children;
ERL_NIF_TERM ATOM_closed;
ERL_NIF_TERM ATOM_configured;
ERL_NIF_TERM ATOM_connected;
ERL_NIF_TERM ATOM_eagain;
ERL_NIF_TERM ATOM_error;
ERL_NIF_TERM ATOM_false;
ERL_NIF_TERM ATOM_finalized;
ERL_NIF_TERM ATOM_gc_avg;
ERL_NIF_TERM ATOM_gc_max;
ERL_NIF_TERM ATOM_gc_min;
ERL_NIF_TERM ATOM_h2o_handler;
ERL_NIF_TERM ATOM_h2o_port;
ERL_NIF_TERM ATOM_h2o_port_closed;
ERL_NIF_TERM ATOM_h2o_port_data;
ERL_NIF_TERM ATOM_hm_stat;
ERL_NIF_TERM ATOM_in_progress;
ERL_NIF_TERM ATOM_listening;
ERL_NIF_TERM ATOM_max;
ERL_NIF_TERM ATOM_mem_info;
ERL_NIF_TERM ATOM_min;
ERL_NIF_TERM ATOM_n_buckets;
ERL_NIF_TERM ATOM_nil;
ERL_NIF_TERM ATOM_num_accept;
ERL_NIF_TERM ATOM_num_children;
ERL_NIF_TERM ATOM_num_listen;
ERL_NIF_TERM ATOM_num_output;
ERL_NIF_TERM ATOM_num_ports;
ERL_NIF_TERM ATOM_ok;
ERL_NIF_TERM ATOM_once;
ERL_NIF_TERM ATOM_open;
ERL_NIF_TERM ATOM_parent;
ERL_NIF_TERM ATOM_ports_stat;
ERL_NIF_TERM ATOM_ready_input;
ERL_NIF_TERM ATOM_seq;
ERL_NIF_TERM ATOM_seq_ports;
ERL_NIF_TERM ATOM_size;
ERL_NIF_TERM ATOM_started;
ERL_NIF_TERM ATOM_state;
ERL_NIF_TERM ATOM_trap;
ERL_NIF_TERM ATOM_true;
ERL_NIF_TERM ATOM_type;
ERL_NIF_TERM ATOM_undefined;

/* NIF Functions */

int
h2o_nif_globals_load(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    if (h2o_nif_port_load(env, nif_data) != 0) {
        return -1;
    }
    if (h2o_nif_slice_load(env, nif_data) != 0) {
        (void)h2o_nif_port_unload(env, nif_data);
        return -1;
    }
    h2o_srand();
    h2o_hostinfo_max_threads = H2O_DEFAULT_NUM_NAME_RESOLUTION_THREADS;
    (void)h2o_sem_init(&h2o_ocsp_updater_semaphore, H2O_DEFAULT_OCSP_UPDATER_MAX_THREADS);

/* Initialize common atoms */
#define ATOM(Id, Value)                                                                                                            \
    {                                                                                                                              \
        Id = enif_make_atom(env, Value);                                                                                           \
    }
    ATOM(ATOM_accept, "accept");
    ATOM(ATOM_active, "active");
    ATOM(ATOM_already_started, "already_started");
    ATOM(ATOM_avg, "avg");
    ATOM(ATOM_badcfg, "badcfg");
    ATOM(ATOM_children, "children");
    ATOM(ATOM_closed, "closed");
    ATOM(ATOM_configured, "configured");
    ATOM(ATOM_connected, "connected");
    ATOM(ATOM_eagain, "eagain");
    ATOM(ATOM_error, "error");
    ATOM(ATOM_false, "false");
    ATOM(ATOM_finalized, "finalized");
    ATOM(ATOM_gc_avg, "gc_avg");
    ATOM(ATOM_gc_max, "gc_max");
    ATOM(ATOM_gc_min, "gc_min");
    ATOM(ATOM_h2o_handler, "h2o_handler");
    ATOM(ATOM_h2o_port, "h2o_port");
    ATOM(ATOM_h2o_port_closed, "h2o_port_closed");
    ATOM(ATOM_h2o_port_data, "h2o_port_data");
    ATOM(ATOM_hm_stat, "hm_stat");
    ATOM(ATOM_in_progress, "in_progress");
    ATOM(ATOM_listening, "listening");
    ATOM(ATOM_max, "max");
    ATOM(ATOM_mem_info, "mem_info");
    ATOM(ATOM_min, "min");
    ATOM(ATOM_n_buckets, "n_buckets");
    ATOM(ATOM_nil, "nil");
    ATOM(ATOM_num_accept, "num_accept");
    ATOM(ATOM_num_children, "num_children");
    ATOM(ATOM_num_listen, "num_listen");
    ATOM(ATOM_num_output, "num_output");
    ATOM(ATOM_num_ports, "num_ports");
    ATOM(ATOM_ok, "ok");
    ATOM(ATOM_once, "once");
    ATOM(ATOM_open, "open");
    ATOM(ATOM_parent, "parent");
    ATOM(ATOM_ports_stat, "ports_stat");
    ATOM(ATOM_ready_input, "ready_input");
    ATOM(ATOM_seq, "seq");
    ATOM(ATOM_seq_ports, "seq_ports");
    ATOM(ATOM_size, "size");
    ATOM(ATOM_started, "started");
    ATOM(ATOM_state, "state");
    ATOM(ATOM_trap, "trap");
    ATOM(ATOM_true, "true");
    ATOM(ATOM_type, "type");
    ATOM(ATOM_undefined, "undefined");
#undef ATOM

    return 0;
}

int
h2o_nif_globals_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    if (h2o_nif_port_upgrade(env, priv_data, old_priv_data, load_info) != 0) {
        return -1;
    }
    if (h2o_nif_slice_upgrade(env, priv_data, old_priv_data, load_info) != 0) {
        return -1;
    }
    return 0;
}

void
h2o_nif_globals_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    (void)h2o_sem_destroy(&h2o_ocsp_updater_semaphore);
    h2o_hostinfo_max_threads = 1;
    (void)h2o_nif_slice_unload(env, nif_data);
    (void)h2o_nif_port_unload(env, nif_data);
    return;
}
