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

/* Global Variables */

extern ck_spinlock_t h2o_nif_ports_spinlock;
extern ErlNifResourceType *h2o_nif_port_resource_type;

/* Types */

typedef struct h2o_nif_port_s h2o_nif_port_t;
typedef struct h2o_nif_port_init_s h2o_nif_port_init_t;
typedef struct h2o_nif_port_sentinel_s h2o_nif_port_sentinel_t;
typedef struct h2o_nif_port_sentinel_ref_s h2o_nif_port_sentinel_ref_t;
typedef struct h2o_nif_port_state_s h2o_nif_port_state_t;

typedef ERL_NIF_TERM h2o_nif_port_stop_t(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
typedef void h2o_nif_port_dtor_t(ErlNifEnv *env, h2o_nif_port_t *port);
typedef void h2o_nif_port_sentinel_dtor_t(h2o_nif_port_t *port);

struct h2o_nif_port_init_s {
    int type;
    h2o_nif_port_stop_t *stop;
    h2o_nif_port_dtor_t *dtor;
};

struct h2o_nif_port_sentinel_s {
    h2o_nif_port_t *port;
    h2o_nif_port_sentinel_dtor_t *dtor;
};

struct h2o_nif_port_sentinel_ref_s {
    int refc;
    h2o_nif_port_sentinel_t *ptr;
};

struct h2o_nif_port_state_s {
    int flags;
    int async;
};

struct h2o_nif_port_s {
    _Atomic h2o_nif_port_state_t state;
    struct {
        ck_spinlock_t lock;
        ErlNifPid pid;
        ErlNifMonitor *mon;
        ErlNifMonitor mons[2];
    } owner;
    h2o_linklist_t _link;
    h2o_nif_port_t *parent;
    h2o_linklist_t children;
    size_t num_children;
    struct {
        int state;
        int quiet;
        h2o_linklist_t children;
        size_t num_children;
        h2o_linklist_t *child;
        void *data;
    } on_close;
    ck_spinlock_t _sentinel_lock;
    h2o_nif_port_sentinel_ref_t _sentinel;
    int type;
    h2o_nif_port_stop_t *stop;
    h2o_nif_port_dtor_t *dtor;
};

/* NIF Functions */

extern int h2o_nif_port_load(ErlNifEnv *env, h2o_nif_data_t *nif_data);
extern int h2o_nif_port_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info);
extern void h2o_nif_port_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data);

/* Resource Functions */

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

extern int h2o_nif_port_open(h2o_nif_port_t *parent, h2o_nif_port_init_t *port_init, size_t size, h2o_nif_port_t **portp);
extern int h2o_nif_port_connect(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifPid new_owner);
extern int h2o_nif_port_sentinel_link(h2o_mem_pool_t *pool, h2o_nif_port_t *port, h2o_nif_port_sentinel_dtor_t *dtor);
extern void h2o_nif_port_sentinel_unlink(h2o_nif_port_t *port);
extern int h2o_nif_port_sentinel_trylock(h2o_nif_port_t *port);
extern int h2o_nif_port_sentinel_tryunlock(h2o_nif_port_t *port);
extern int h2o_nif_port_stop(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
extern int h2o_nif_port_stop_quiet(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
extern int h2o_nif_port_stop_continue(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out);
static ErlNifPid h2o_nif_port_get_owner(h2o_nif_port_t *port);
static int h2o_nif_port_send(ErlNifEnv *env, h2o_nif_port_t *port, ErlNifEnv *msg_env, ERL_NIF_TERM msg);

inline ErlNifPid
h2o_nif_port_get_owner(h2o_nif_port_t *port)
{
    ErlNifPid pid;
    (void)ck_spinlock_lock_eb(&port->owner.lock);
    pid = port->owner.pid;
    (void)ck_spinlock_unlock(&port->owner.lock);
    return pid;
}

inline int
h2o_nif_port_send(ErlNifEnv *env, h2o_nif_port_t *port, ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    ErlNifPid pid;
    pid = h2o_nif_port_get_owner(port);
    return enif_send(env, &pid, msg_env, msg);
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
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return (state.flags == H2O_NIF_PORT_STATE_CLOSED);
}

inline int
h2o_nif_port_is_open(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN);
}

inline int
h2o_nif_port_is_configured(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_CONFIGURED) == H2O_NIF_PORT_STATE_CONFIGURED);
}

inline int
h2o_nif_port_is_started(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED);
}

inline int
h2o_nif_port_is_listening(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_LISTENING) == H2O_NIF_PORT_STATE_LISTENING);
}

inline int
h2o_nif_port_is_requested(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_REQUESTED) == H2O_NIF_PORT_STATE_REQUESTED);
}

inline int
h2o_nif_port_is_send_data(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_SEND_DATA) == H2O_NIF_PORT_STATE_SEND_DATA);
}

inline int
h2o_nif_port_is_finalized(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    return ((state.flags & H2O_NIF_PORT_STATE_FINALIZED) == H2O_NIF_PORT_STATE_FINALIZED);
}

inline ERL_NIF_TERM
h2o_nif_port_state_to_atom(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t state = atomic_load_explicit(&port->state, memory_order_relaxed);
    ERL_NIF_TERM value;
    if (state.flags == H2O_NIF_PORT_STATE_CLOSED) {
        value = ATOM_closed;
    } else if ((state.flags & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED) {
        value = ATOM_started;
    } else if ((state.flags & H2O_NIF_PORT_STATE_CONFIGURED) == H2O_NIF_PORT_STATE_CONFIGURED) {
        value = ATOM_configured;
    } else if ((state.flags & H2O_NIF_PORT_STATE_LISTENING) == H2O_NIF_PORT_STATE_LISTENING) {
        value = ATOM_listening;
    } else if ((state.flags & H2O_NIF_PORT_STATE_SEND_DATA) == H2O_NIF_PORT_STATE_SEND_DATA) {
        value = ATOM_send_data;
    } else if ((state.flags & H2O_NIF_PORT_STATE_REQUESTED) == H2O_NIF_PORT_STATE_REQUESTED) {
        value = ATOM_requested;
    } else if ((state.flags & H2O_NIF_PORT_STATE_FINALIZED) == H2O_NIF_PORT_STATE_FINALIZED) {
        value = ATOM_finalized;
    } else if ((state.flags & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) {
        value = ATOM_open;
    } else {
        value = ATOM_undefined;
    }
    return value;
}

/* State Transition Functions */

// static int __h2o_nif_port_compare_and_swap_state(h2o_nif_port_t *port, int expected, int desired);
// static int __h2o_nif_port_fetch_and_swap_state(h2o_nif_port_t *port, int desired);
static int h2o_nif_port_state_init(h2o_nif_port_t *port, int state);
static int h2o_nif_port_state_transition(h2o_nif_port_t *port, int old_state, int new_state);
static int h2o_nif_port_set_open(h2o_nif_port_t *port);
static int h2o_nif_port_set_configured(h2o_nif_port_t *port);
static int h2o_nif_port_set_started(h2o_nif_port_t *port);
static int h2o_nif_port_set_listening(h2o_nif_port_t *port);
static int h2o_nif_port_set_requested(h2o_nif_port_t *port);
static int h2o_nif_port_add_requested(h2o_nif_port_t *port);
static int h2o_nif_port_sub_requested(h2o_nif_port_t *port);
static int h2o_nif_port_add_send_data(h2o_nif_port_t *port);
static int h2o_nif_port_sub_send_data(h2o_nif_port_t *port);
static int h2o_nif_port_add_finalized(h2o_nif_port_t *port);
static int h2o_nif_port_sub_finalized(h2o_nif_port_t *port);

// inline int
// __h2o_nif_port_compare_and_swap_state(h2o_nif_port_t *port, int expected, int desired)
// {
//     return (atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed));
// }

// inline int
// __h2o_nif_port_fetch_and_swap_state(h2o_nif_port_t *port, int desired)
// {
//     int expected;
//     int fetched;
//     int result;
//     do {
//         expected = fetched = atomic_load_explicit(&port->state, memory_order_relaxed);
//         if (expected == H2O_NIF_PORT_STATE_CLOSED) {
//             return H2O_NIF_PORT_STATE_CLOSED;
//         }
//         result = __h2o_nif_port_compare_and_swap_state(port, expected, desired);
//         if (!result) {
//             (void)ck_pr_stall();
//         }
//     } while (!result);
//     return fetched;
// }

inline int
h2o_nif_port_state_init(h2o_nif_port_t *port, int state)
{
    h2o_nif_port_state_t expected = {H2O_NIF_PORT_STATE_ALLOCATED, 0};
    h2o_nif_port_state_t desired = {state, 0};
    return atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
}

inline int
h2o_nif_port_state_transition(h2o_nif_port_t *port, int old_state, int new_state)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != old_state) {
            return 0;
        }
        desired.flags = new_state;
        desired.async = expected.async;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return result;
}

inline int
h2o_nif_port_set_open(h2o_nif_port_t *port)
{
    return h2o_nif_port_state_init(port, H2O_NIF_PORT_STATE_OPEN);
}

inline int
h2o_nif_port_set_configured(h2o_nif_port_t *port)
{
    return h2o_nif_port_state_transition(port, H2O_NIF_PORT_STATE_OPEN, H2O_NIF_PORT_STATE_CONFIGURED);
}

inline int
h2o_nif_port_set_started(h2o_nif_port_t *port)
{
    return h2o_nif_port_state_transition(port, H2O_NIF_PORT_STATE_CONFIGURED, H2O_NIF_PORT_STATE_STARTED);
}

inline int
h2o_nif_port_set_listening(h2o_nif_port_t *port)
{
    return h2o_nif_port_state_init(port, H2O_NIF_PORT_STATE_LISTENING);
}

inline int
h2o_nif_port_set_requested(h2o_nif_port_t *port)
{
    return h2o_nif_port_state_init(port, H2O_NIF_PORT_STATE_REQUESTED);
}

inline int
h2o_nif_port_add_requested(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_REQUESTED && expected.flags != H2O_NIF_PORT_STATE_SEND_DATA) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = expected.flags;
        desired.async = expected.async + 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

inline int
h2o_nif_port_sub_requested(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_REQUESTED && expected.flags != H2O_NIF_PORT_STATE_SEND_DATA &&
            expected.flags != H2O_NIF_PORT_STATE_FINALIZED) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = expected.flags;
        desired.async = expected.async - 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

inline int
h2o_nif_port_add_send_data(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_REQUESTED && expected.flags != H2O_NIF_PORT_STATE_SEND_DATA) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = H2O_NIF_PORT_STATE_SEND_DATA;
        desired.async = expected.async + 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

inline int
h2o_nif_port_sub_send_data(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_SEND_DATA && expected.flags != H2O_NIF_PORT_STATE_FINALIZED) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = expected.flags;
        desired.async = expected.async - 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

inline int
h2o_nif_port_add_finalized(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_REQUESTED && expected.flags != H2O_NIF_PORT_STATE_SEND_DATA) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = H2O_NIF_PORT_STATE_FINALIZED;
        desired.async = expected.async + 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

inline int
h2o_nif_port_sub_finalized(h2o_nif_port_t *port)
{
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int old_state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected.flags != H2O_NIF_PORT_STATE_FINALIZED) {
            return 0;
        }
        old_state = expected.flags;
        desired.flags = expected.flags;
        desired.async = expected.async - 1;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return old_state;
}

#endif
