// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_PORT_H
#define H2O_NIF_PORT_H

#include "globals.h"

#define H2O_NIF_PORT_ACTIVE_FALSE 0
#define H2O_NIF_PORT_ACTIVE_ONCE 1
#define H2O_NIF_PORT_ACTIVE_TRUE 2

#define H2O_NIF_PORT_FLAG_ALC 0x0001
#define H2O_NIF_PORT_FLAG_OPN 0x0002
#define H2O_NIF_PORT_FLAG_CFG 0x0004
#define H2O_NIF_PORT_FLAG_SRT 0x0008
#define H2O_NIF_PORT_FLAG_LST 0x0010
#define H2O_NIF_PORT_FLAG_ACC 0x0020
#define H2O_NIF_PORT_FLAG_FIN 0x0040

#define H2O_NIF_PORT_STATE_CLOSED (0)
#define H2O_NIF_PORT_STATE_ALLOCATED (H2O_NIF_PORT_FLAG_ALC)
#define H2O_NIF_PORT_STATE_OPEN (H2O_NIF_PORT_STATE_ALLOCATED | H2O_NIF_PORT_FLAG_OPN)
#define H2O_NIF_PORT_STATE_CONFIGURED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_CFG)
#define H2O_NIF_PORT_STATE_STARTED (H2O_NIF_PORT_STATE_CONFIGURED | H2O_NIF_PORT_FLAG_SRT)
#define H2O_NIF_PORT_STATE_LISTENING (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_LST)
#define H2O_NIF_PORT_STATE_ACCEPTING (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_ACC)
#define H2O_NIF_PORT_STATE_FINISHED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_FIN)

#define H2O_NIF_PORT_TYPE_NONE 0
#define H2O_NIF_PORT_TYPE_SERVER 1
#define H2O_NIF_PORT_TYPE_FILTER 2
#define H2O_NIF_PORT_TYPE_HANDLER 3
#define H2O_NIF_PORT_TYPE_LOGGER 4
#define H2O_NIF_PORT_TYPE_REQUEST 5

// #define H2O_NIF_PORT_IS_OPEN(p) (((p)->state & H2O_NIF_PORT_FLAG_OPEN) == H2O_NIF_PORT_FLAG_OPEN)

// #define H2O_NIF_PORT_IS_CONFIGURED(p) (((p)->state & H2O_NIF_PORT_STATE_CONFIGURED) == H2O_NIF_PORT_STATE_CONFIGURED)

// #define H2O_NIF_PORT_IS_STARTED(p) (((p)->state & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED)

// typedef enum h2o_nif_port_trap_t {
//     H2O_NIF_PORT_TRAP_PART = 1,
//     H2O_NIF_PORT_TRAP_DONE
// } h2o_nif_port_trap_t;

// typedef struct h2o_nif_port_trap_ctx_s h2o_nif_port_trap_ctx_t;

// typedef h2o_nif_port_trap_t h2o_nif_port_trap_callback_t(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_port_trap_ctx_t *ctx);

// struct h2o_nif_port_trap_ctx_s {
//     int state;
//     h2o_nif_port_trap_callback_t *callback;
//     void *data;
// };

// typedef struct h2o_nif_port_state_s {
//     int active;
//     int16_t active_count;
// } h2o_nif_port_state_t;

extern ck_spinlock_t h2o_nif_ports_spinlock;

// typedef struct h2o_nif_port_outbox_s {
//     int aba;
//     int active;
//     int16_t count;
// } h2o_nif_port_outbox_t;

// _Atomic int num_output;
// ck_fifo_mpmc_t output;
// _Atomic int num_accept;
// ck_fifo_mpmc_t accept;
// _Atomic int num_listen;
// ck_fifo_mpmc_t listen;

// extern int h2o_nif_port_open(h2o_nif_port_t *parent, h2o_nif_port_t **portp);

typedef struct h2o_nif_port_accept_s {
    h2o_linklist_t _link;
    unsigned long id;
    ErlNifPid caller;
    ErlNifEnv *msg_env;
} h2o_nif_port_accept_t;

typedef struct h2o_nif_port_listen_s {
    h2o_linklist_t _link;
} h2o_nif_port_listen_t;

typedef int h2o_nif_port_on_accept_t(h2o_nif_port_t *parent, h2o_nif_port_listen_t *listen, h2o_nif_port_t **portp);

struct h2o_nif_port_s {
    h2o_linklist_t _link;
    uintptr_t id;
    _Atomic int state;
    _Atomic ErlNifPid owner;
    h2o_nif_port_t *parent;
    h2o_linklist_t children;
    _Atomic int num_children;
    _Atomic int active;
    _Atomic int num_output;
    _Atomic int num_listen;
    _Atomic int num_accept;
    ck_spinlock_t spinlock_accept;
    ck_fifo_mpmc_t output;
    h2o_linklist_t accept;
    h2o_linklist_t listen;
    int has_garbage;
    h2o_nif_return_ctx_t on_close;
    h2o_nif_port_on_accept_t *on_accept;
    int type;
    void *data;
    // int type;
    // void *data;
    // _Atomic h2o_nif_port_outbox_t outbox;
    // ck_fifo_mpmc_t outbox;
    // ck_spinlock_t spinlock;
    // ErlNifRWLock *rwlock;
    // h2o_nif_port_t *parent;
    // h2o_linklist_t children;
    // int id;
    // ErlNifPid owner;
    // int state;
    // int active;
    // int16_t active_count;
    // volatile sig_atomic_t opened;
    // volatile sig_atomic_t closed;
    // volatile sig_atomic_t released;
    // volatile sig_atomic_t release_count;
    // int type;
    // h2o_nif_port_on_close_t *on_close;
    // void *data;
    // h2o_nif_queue_t acc;
    // h2o_nif_queue_t req;
};

// struct h2o_nif_port_s {
//     h2o_linklist_t _link;
//     // ErlNifRWLock *rwlock;
//     h2o_nif_port_t *parent;
//     h2o_linklist_t children;
//     int id;
//     ErlNifPid owner;
//     int state;
//     int active;
//     int16_t active_count;
//     volatile sig_atomic_t opened;
//     volatile sig_atomic_t closed;
//     volatile sig_atomic_t released;
//     volatile sig_atomic_t release_count;
//     int type;
//     h2o_nif_port_on_close_t *on_close;
//     void *data;
//     // h2o_nif_queue_t acc;
//     // h2o_nif_queue_t req;
// };

typedef struct h2o_nif_ports_stat_s {
    size_t gc_avg;
    size_t gc_max;
    size_t gc_min;
    size_t seq;
} h2o_nif_ports_stat_t;

// extern ERL_NIF_TERM h2o_nif_port_mem_info(ErlNifEnv *env);
extern int h2o_nif_ports_load(h2o_nif_data_t *nif_data);
extern void h2o_nif_ports_unload(void);
extern int h2o_nif_ports_stat(h2o_nif_ports_stat_t *stat);
extern h2o_nif_port_t *h2o_nif_port_alloc(void);
extern int h2o_nif_port_open(h2o_nif_port_t *parent, h2o_nif_port_t **portp);
static int h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp);
static void h2o_nif_port_keep(h2o_nif_port_t *port);
static void h2o_nif_port_release(h2o_nif_port_t *port);
// extern int h2o_nif_port_get_by_id(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp);
// extern void h2o_nif_port_release(h2o_nif_port_t *port);
extern int h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port, H2O_NIF_RETURN *nif_return);
extern int _h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port, H2O_NIF_RETURN *nif_return);
// extern int h2o_nif_port_refc(h2o_nif_port_t *port);
// extern int h2o_nif_port_release(h2o_nif_port_t *port);
extern void h2o_nif_port_dtor(ErlNifEnv *env, void *obj);
/* Listen/Accept Functions */
extern int h2o_nif_port_listen_dispatch(h2o_nif_port_t *port, h2o_nif_port_listen_t *port_listen);
extern int h2o_nif_port_accept(ErlNifEnv *env, h2o_nif_port_t *port, ERL_NIF_TERM *out);
/* Output Functions */
extern int h2o_nif_port_output_reserve_send(h2o_nif_port_t *port);
extern int h2o_nif_port_output_send(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifEnv *msg_env, ERL_NIF_TERM msg);
extern int h2o_nif_port_output_term(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifEnv *msg_env, ERL_NIF_TERM msg);
extern int h2o_nif_port_output_flush(ErlNifEnv *env, h2o_nif_port_t *port);
static int h2o_nif_port_output_enqueue(h2o_nif_port_t *port, h2o_nif_msg_t *msg);

extern int h2o_nif_port_gc_start(void);
// extern int h2o_nif_port_checkout(ErlNifEnv *env, ERL_NIF_TERM port_id_term, h2o_nif_port_t **portp, int readwrite);
// extern void h2o_nif_port_checkin(h2o_nif_port_t **portp, int readwrite);
// extern ERL_NIF_TERM h2o_nif_port_open(ErlNifEnv *env, h2o_nif_port_t **newport);
// extern ERL_NIF_TERM h2o_nif_port_close(ErlNifEnv *env, ERL_NIF_TERM port_id_term);
// extern ERL_NIF_TERM h2o_nif_port_spawn(ErlNifEnv *env, h2o_nif_port_t *parent, h2o_nif_port_t **newport);

static int h2o_nif_port_set_owner(h2o_nif_port_t *port, ErlNifPid owner);

static inline int
h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_port_t **portp)
{
    h2o_nif_port_t *port = NULL;
    const ERL_NIF_TERM *tuple;
    int arity;
    uintptr_t port_id;
    if (portp != NULL) {
        *portp = NULL;
    }
    if (!enif_get_tuple(env, port_term, &arity, &tuple) || arity != 2 || tuple[0] != ATOM_h2o_port ||
        !enif_get_ulong(env, tuple[1], &port_id) || !h2o_nif_hm_get(port_id, (uintptr_t *)&port, (portp != NULL))) {
        return 0;
    }
    if (portp != NULL) {
        *portp = port;
    }
    return 1;
}

static inline void
h2o_nif_port_keep(h2o_nif_port_t *port)
{
    (void)enif_keep_resource((void *)port);
}

static inline void
h2o_nif_port_release(h2o_nif_port_t *port)
{
    (void)enif_release_resource((void *)port);
}

static inline int
h2o_nif_port_set_owner(h2o_nif_port_t *port, ErlNifPid owner)
{
    if (port == NULL) {
        return 0;
    }
    (void)atomic_store_explicit(&port->owner, owner, memory_order_relaxed);
    return 1;
}

static inline int
h2o_nif_port_output_enqueue(h2o_nif_port_t *port, h2o_nif_msg_t *msg)
{
    ck_fifo_mpmc_entry_t *fifo_entry = (ck_fifo_mpmc_entry_t *)malloc(sizeof(*fifo_entry));
    if (fifo_entry == NULL) {
        return 0;
    }
    (void)memset(fifo_entry, 0, sizeof(*fifo_entry));
    // (void)ck_fifo_mpmc_enqueue(&port->outq, fifo_entry, (void *)msg);
    while (ck_fifo_mpmc_tryenqueue(&port->output, fifo_entry, (void *)msg) == false) {
        (void)ck_pr_stall();
    }
    (void)atomic_fetch_add_explicit(&port->num_output, 1, memory_order_relaxed);
    return 1;
}

/* State Functions */
static int h2o_nif_port_finish(h2o_nif_port_t *port);
static int h2o_nif_port_is_closed(h2o_nif_port_t *port);
static int h2o_nif_port_is_allocated(h2o_nif_port_t *port);
static int h2o_nif_port_is_open(h2o_nif_port_t *port);
static int h2o_nif_port_is_configured(h2o_nif_port_t *port);
static int h2o_nif_port_is_started(h2o_nif_port_t *port);
static int h2o_nif_port_is_listening(h2o_nif_port_t *port);
static int h2o_nif_port_is_accepting(h2o_nif_port_t *port);
static int h2o_nif_port_cas_or_state(h2o_nif_port_t *port, int state);
static int h2o_nif_port_cas_set_state(h2o_nif_port_t *port, int state);
static int h2o_nif_port_fetch_set_state(h2o_nif_port_t *port, int state);

static ERL_NIF_TERM h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port);
// static int h2o_nif_port_rlock(h2o_nif_port_t *port);
// static int h2o_nif_port_runlock(h2o_nif_port_t *port);
// static int h2o_nif_port_rwlock(h2o_nif_port_t *port);
// static int h2o_nif_port_rwunlock(h2o_nif_port_t *port);
// static int h2o_nif_port_tryclose(h2o_nif_port_t *port);
// static int h2o_nif_port_tryrelease(h2o_nif_port_t *port);

inline int
h2o_nif_port_finish(h2o_nif_port_t *port)
{
    int expected = H2O_NIF_PORT_STATE_ACCEPTING;
    int desired = H2O_NIF_PORT_STATE_FINISHED;
    return atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
}

static inline int
h2o_nif_port_is_closed(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return (atomic_load_explicit(&port->state, memory_order_relaxed) == H2O_NIF_PORT_STATE_CLOSED);
}

static inline int
h2o_nif_port_is_allocated(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_ALLOCATED) ==
            H2O_NIF_PORT_STATE_ALLOCATED);
}

static inline int
h2o_nif_port_is_open(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN);
}

static inline int
h2o_nif_port_is_configured(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_CONFIGURED) ==
            H2O_NIF_PORT_STATE_CONFIGURED);
}

static inline int
h2o_nif_port_is_started(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED);
}

static inline int
h2o_nif_port_is_listening(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_LISTENING) ==
            H2O_NIF_PORT_STATE_LISTENING);
}

static inline int
h2o_nif_port_is_accepting(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 1;
    }
    return ((atomic_load_explicit(&port->state, memory_order_relaxed) & H2O_NIF_PORT_STATE_ACCEPTING) ==
            H2O_NIF_PORT_STATE_ACCEPTING);
}

static inline int
h2o_nif_port_cas_or_state(h2o_nif_port_t *port, int state)
{
    int expected;
    int desired;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected == H2O_NIF_PORT_STATE_CLOSED) {
            return 0;
        }
        desired = expected | state;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return result;
}

static inline int
h2o_nif_port_cas_set_state(h2o_nif_port_t *port, int state)
{
    int expected;
    int desired = state;
    int result;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected == H2O_NIF_PORT_STATE_CLOSED) {
            return 0;
        }
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return result;
}

static inline int
h2o_nif_port_fetch_set_state(h2o_nif_port_t *port, int state)
{
    int expected;
    int desired = state;
    int result;
    int fetched;
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        if (expected == H2O_NIF_PORT_STATE_CLOSED) {
            return 0;
        }
        fetched = expected;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return fetched;
}

static inline ERL_NIF_TERM
h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port)
{
    if (port == NULL) {
        return enif_make_badarg(env);
    }
    return enif_make_tuple2(env, ATOM_h2o_port, enif_make_ulong(env, port->id));
}

// static inline int
// h2o_nif_port_rlock(h2o_nif_port_t *port)
// {
//     if (port == NULL || port->rwlock == NULL) {
//         return 0;
//     }
//     (void)enif_rwlock_rlock(port->rwlock);
//     return 1;
// }

// static inline int
// h2o_nif_port_runlock(h2o_nif_port_t *port)
// {
//     if (port == NULL || port->rwlock == NULL) {
//         return 0;
//     }
//     (void)enif_rwlock_runlock(port->rwlock);
//     return 1;
// }

// static inline int
// h2o_nif_port_rwlock(h2o_nif_port_t *port)
// {
//     if (port == NULL || port->rwlock == NULL) {
//         return 0;
//     }
//     (void)enif_rwlock_rwlock(port->rwlock);
//     return 1;
// }

// static inline int
// h2o_nif_port_rwunlock(h2o_nif_port_t *port)
// {
//     if (port == NULL || port->rwlock == NULL) {
//         return 0;
//     }
//     (void)enif_rwlock_rwunlock(port->rwlock);
//     return 1;
// }

// static inline int
// h2o_nif_port_tryclose(h2o_nif_port_t *port)
// {
//     if (port == NULL) {
//         return 0;
//     }
//     if (!__sync_bool_compare_and_swap(&port->closed, 0, 1)) {
//         return 0;
//     }
//     if (!h2o_nif_port_rwlock(port)) {
//         TRACE_F("Unable to obtain rwlock while closing port\n");
//         return 0;
//     }
//     return 1;
// }

// static inline int
// h2o_nif_port_tryrelease(h2o_nif_port_t *port)
// {
//     if (port == NULL) {
//         return 0;
//     }
//     if (!__sync_bool_compare_and_swap(&port->released, 0, 1)) {
//         return 0;
//     }
//     return 1;
// }

#endif
