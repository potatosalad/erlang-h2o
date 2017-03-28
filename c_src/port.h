// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_PORT_H
#define H2O_NIF_PORT_H

#include "globals.h"

#define H2O_NIF_PORT_FLAG_ALC 0x0001
#define H2O_NIF_PORT_FLAG_OPN 0x0002
#define H2O_NIF_PORT_FLAG_CFG 0x0004
#define H2O_NIF_PORT_FLAG_SRT 0x0008
#define H2O_NIF_PORT_FLAG_LST 0x0010
#define H2O_NIF_PORT_FLAG_REQ 0x0020
#define H2O_NIF_PORT_FLAG_DAT 0x0040
#define H2O_NIF_PORT_FLAG_FIN 0x0080

#define H2O_NIF_PORT_STATE_CLOSED (0)
#define H2O_NIF_PORT_STATE_ALLOCATED (H2O_NIF_PORT_FLAG_ALC)
#define H2O_NIF_PORT_STATE_OPEN (H2O_NIF_PORT_STATE_ALLOCATED | H2O_NIF_PORT_FLAG_OPN)
#define H2O_NIF_PORT_STATE_CONFIGURED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_CFG)
#define H2O_NIF_PORT_STATE_STARTED (H2O_NIF_PORT_STATE_CONFIGURED | H2O_NIF_PORT_FLAG_SRT)
#define H2O_NIF_PORT_STATE_LISTENING (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_LST)
#define H2O_NIF_PORT_STATE_REQUESTED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_REQ)
#define H2O_NIF_PORT_STATE_SEND_DATA (H2O_NIF_PORT_STATE_REQUESTED | H2O_NIF_PORT_FLAG_DAT)
#define H2O_NIF_PORT_STATE_FINALIZED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_FIN)

#define H2O_NIF_PORT_TYPE_NONE 0
#define H2O_NIF_PORT_TYPE_SERVER 1
#define H2O_NIF_PORT_TYPE_FILTER 2
#define H2O_NIF_PORT_TYPE_HANDLER 3
#define H2O_NIF_PORT_TYPE_LOGGER 4
#define H2O_NIF_PORT_TYPE_REQUEST 5
#define H2O_NIF_PORT_TYPE_FILTER_EVENT 6
#define H2O_NIF_PORT_TYPE_HANDLER_EVENT 7

/* Variables */

extern ck_spinlock_t h2o_nif_ports_spinlock;
extern ErlNifResourceType *h2o_nif_port_resource_type;

/* Types */

typedef struct h2o_nif_port_s h2o_nif_port_t;

typedef ERL_NIF_TERM h2o_nif_port_on_close_t(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
typedef void h2o_nif_port_on_dtor_t(ErlNifEnv *env, h2o_nif_port_t *port);

struct h2o_nif_port_s {
    h2o_linklist_t _link;
    _Atomic int state;
    _Atomic ErlNifPid owner;
    h2o_nif_port_t *parent;
    h2o_linklist_t children;
    _Atomic int num_children;
    struct {
        int state;
        int silent;
        h2o_nif_port_on_close_t *callback;
        void *data;
    } on_close;
    h2o_nif_port_on_dtor_t *dtor;
    int type;
};

/* NIF Functions */

extern int h2o_nif_port_load(ErlNifEnv *env, h2o_nif_data_t *nif_data);
extern int h2o_nif_port_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
extern void h2o_nif_port_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data);

/* Resource Functions */

extern h2o_nif_port_t *h2o_nif_port_alloc(size_t size);
extern void h2o_nif_port_dtor(ErlNifEnv *env, void *obj);
static int h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_port_t **portp);
static void h2o_nif_port_keep(h2o_nif_port_t *port);
static ERL_NIF_TERM h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port);
static void h2o_nif_port_release(h2o_nif_port_t *port);

inline int
h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_port_t **portp)
{
    assert(portp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!enif_get_resource(env, port_term, h2o_nif_port_resource_type, (void **)&port)) {
        *portp = NULL;
        return 0;
    }
    *portp = port;
    return 1;
}

inline void
h2o_nif_port_keep(h2o_nif_port_t *port)
{
    (void)enif_keep_resource((void *)port);
}

inline ERL_NIF_TERM
h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port)
{
    return enif_make_resource(env, (void *)port);
}

inline void
h2o_nif_port_release(h2o_nif_port_t *port)
{
    (void)enif_release_resource((void *)port);
}

/* Port Functions */

extern int h2o_nif_port_open(h2o_nif_port_t *parent, size_t size, h2o_nif_port_t **portp);
extern int h2o_nif_port_close(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
extern int h2o_nif_port_close_silent(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
extern int __h2o_nif_port_close(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
static void h2o_nif_port_connect(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifPid new_owner);
static ErlNifPid h2o_nif_port_get_owner(h2o_nif_port_t *port);
static void h2o_nif_port_set_owner(h2o_nif_port_t *port, ErlNifPid new_owner);
static int h2o_nif_port_send(ErlNifEnv *env, h2o_nif_port_t *port, ErlNifEnv *msg_env, ERL_NIF_TERM msg);

inline void
h2o_nif_port_connect(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifPid new_owner)
{
    (void)h2o_nif_port_set_owner(port, new_owner);
    if (atomic_load_explicit(&port->state, memory_order_relaxed) == H2O_NIF_PORT_STATE_CLOSED) {
        if (((port->on_close.state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) && !port->on_close.silent) {
            ErlNifEnv *msg_env = (env != NULL) ? env : enif_alloc_env();
            ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
            if (env != NULL) {
                (void)h2o_nif_port_send(msg_env, port, NULL, msg);
            } else {
                (void)h2o_nif_port_send(NULL, port, msg_env, msg);
                (void)enif_free_env(msg_env);
            }
        }
    }
}

inline ErlNifPid
h2o_nif_port_get_owner(h2o_nif_port_t *port)
{
    return (atomic_load_explicit(&port->owner, memory_order_relaxed));
}

inline void
h2o_nif_port_set_owner(h2o_nif_port_t *port, ErlNifPid new_owner)
{
    (void)atomic_store_explicit(&port->owner, new_owner, memory_order_relaxed);
}

inline int
h2o_nif_port_send(ErlNifEnv *env, h2o_nif_port_t *port, ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    ErlNifPid owner = h2o_nif_port_get_owner(port);
    return enif_send(env, &owner, msg_env, msg);
}

/* State Functions */

static int h2o_nif_port_is_closed(h2o_nif_port_t *port);
static int h2o_nif_port_is_open(h2o_nif_port_t *port);
static int h2o_nif_port_is_configured(h2o_nif_port_t *port);
static int h2o_nif_port_is_started(h2o_nif_port_t *port);
static int h2o_nif_port_is_listening(h2o_nif_port_t *port);
static int h2o_nif_port_is_requested(h2o_nif_port_t *port);
static int h2o_nif_port_is_send_data(h2o_nif_port_t *port);
static int h2o_nif_port_is_finalized(h2o_nif_port_t *port);
static ERL_NIF_TERM h2o_nif_port_state_to_atom(h2o_nif_port_t *port);

static inline int
h2o_nif_port_is_closed(h2o_nif_port_t *port)
{
    return (atomic_load_explicit(&port->state, memory_order_relaxed) == H2O_NIF_PORT_STATE_CLOSED);
}

inline int
h2o_nif_port_is_open(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN);
}

inline int
h2o_nif_port_is_configured(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_CONFIGURED) ==
            H2O_NIF_PORT_STATE_CONFIGURED);
}

inline int
h2o_nif_port_is_started(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED);
}

inline int
h2o_nif_port_is_listening(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_LISTENING) ==
            H2O_NIF_PORT_STATE_LISTENING);
}

inline int
h2o_nif_port_is_requested(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_REQUESTED) ==
            H2O_NIF_PORT_STATE_REQUESTED);
}

inline int
h2o_nif_port_is_send_data(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_SEND_DATA) ==
            H2O_NIF_PORT_STATE_SEND_DATA);
}

inline int
h2o_nif_port_is_finalized(h2o_nif_port_t *port)
{
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_FINALIZED) ==
            H2O_NIF_PORT_STATE_FINALIZED);
}

inline ERL_NIF_TERM
h2o_nif_port_state_to_atom(h2o_nif_port_t *port)
{
    int state = atomic_load_explicit(&port->state, memory_order_relaxed);
    ERL_NIF_TERM value;
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        value = ATOM_closed;
    } else if ((state & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED) {
        value = ATOM_started;
    } else if ((state & H2O_NIF_PORT_STATE_CONFIGURED) == H2O_NIF_PORT_STATE_CONFIGURED) {
        value = ATOM_configured;
    } else if ((state & H2O_NIF_PORT_STATE_LISTENING) == H2O_NIF_PORT_STATE_LISTENING) {
        value = ATOM_listening;
    } else if ((state & H2O_NIF_PORT_STATE_SEND_DATA) == H2O_NIF_PORT_STATE_SEND_DATA) {
        value = ATOM_send_data;
    } else if ((state & H2O_NIF_PORT_STATE_REQUESTED) == H2O_NIF_PORT_STATE_REQUESTED) {
        value = ATOM_requested;
    } else if ((state & H2O_NIF_PORT_STATE_FINALIZED) == H2O_NIF_PORT_STATE_FINALIZED) {
        value = ATOM_finalized;
    } else if ((state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) {
        value = ATOM_open;
    } else {
        value = ATOM_undefined;
    }
    return value;
}

/* State Transition Functions */

static int __h2o_nif_port_compare_and_swap_state(h2o_nif_port_t *port, int expected, int desired);
static int __h2o_nif_port_fetch_and_swap_state(h2o_nif_port_t *port, int desired);
static int h2o_nif_port_set_open(h2o_nif_port_t *port);
static int h2o_nif_port_set_configured(h2o_nif_port_t *port);
static int h2o_nif_port_set_started(h2o_nif_port_t *port);
static int h2o_nif_port_set_listening(h2o_nif_port_t *port);
static int h2o_nif_port_set_requested(h2o_nif_port_t *port);
static int h2o_nif_port_set_send_data(h2o_nif_port_t *port);
static int h2o_nif_port_set_finalized(h2o_nif_port_t *port);

inline int
__h2o_nif_port_compare_and_swap_state(h2o_nif_port_t *port, int expected, int desired)
{
    return (atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed));
}

inline int
__h2o_nif_port_fetch_and_swap_state(h2o_nif_port_t *port, int desired)
{
    int expected;
    int fetched;
    int result;
    do {
        expected = fetched = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected == H2O_NIF_PORT_STATE_CLOSED) {
            return H2O_NIF_PORT_STATE_CLOSED;
        }
        result = __h2o_nif_port_compare_and_swap_state(port, expected, desired);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return fetched;
}

inline int
h2o_nif_port_set_open(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_ALLOCATED, H2O_NIF_PORT_STATE_OPEN));
}

inline int
h2o_nif_port_set_configured(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_OPEN, H2O_NIF_PORT_STATE_CONFIGURED));
}

inline int
h2o_nif_port_set_started(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_CONFIGURED, H2O_NIF_PORT_STATE_STARTED));
}

inline int
h2o_nif_port_set_listening(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_ALLOCATED, H2O_NIF_PORT_STATE_LISTENING));
}

inline int
h2o_nif_port_set_requested(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_ALLOCATED, H2O_NIF_PORT_STATE_REQUESTED));
}

inline int
h2o_nif_port_set_send_data(h2o_nif_port_t *port)
{
    return (__h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_REQUESTED, H2O_NIF_PORT_STATE_SEND_DATA));
}

inline int
h2o_nif_port_set_finalized(h2o_nif_port_t *port)
{
    int result;
    result = __h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_REQUESTED, H2O_NIF_PORT_STATE_FINALIZED);
    if (!result) {
        result = __h2o_nif_port_compare_and_swap_state(port, H2O_NIF_PORT_STATE_SEND_DATA, H2O_NIF_PORT_STATE_FINALIZED);
    }
    return result;
}

#endif
