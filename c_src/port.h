// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_PORT_H
#define H2O_NIF_PORT_H

#include "globals.h"

#define H2O_NIF_PORT_ACTIVE_FALSE 0
#define H2O_NIF_PORT_ACTIVE_TRUE 1
#define H2O_NIF_PORT_ACTIVE_ONCE 2
#define H2O_NIF_PORT_ACTIVE_MULTI 3

#define H2O_NIF_PORT_FLAG_OPEN 0x0001
#define H2O_NIF_PORT_FLAG_CONFIG 0x0002
#define H2O_NIF_PORT_FLAG_START 0x0004

#define H2O_NIF_PORT_STATE_CLOSED (0)
#define H2O_NIF_PORT_STATE_OPEN (H2O_NIF_PORT_FLAG_OPEN)
#define H2O_NIF_PORT_STATE_CONFIGURED (H2O_NIF_PORT_STATE_OPEN | H2O_NIF_PORT_FLAG_CONFIG)
#define H2O_NIF_PORT_STATE_STARTED (H2O_NIF_PORT_STATE_CONFIGURED | H2O_NIF_PORT_FLAG_START)

#define H2O_NIF_PORT_TYPE_NONE 0
#define H2O_NIF_PORT_TYPE_SERVER 1

#define H2O_NIF_PORT_IS_OPEN(p) (((p)->state & H2O_NIF_PORT_FLAG_OPEN) == H2O_NIF_PORT_FLAG_OPEN)

#define H2O_NIF_PORT_IS_CONFIGURED(p) (((p)->state & H2O_NIF_PORT_STATE_CONFIGURED) == H2O_NIF_PORT_STATE_CONFIGURED)

#define H2O_NIF_PORT_IS_STARTED(p) (((p)->state & H2O_NIF_PORT_STATE_STARTED) == H2O_NIF_PORT_STATE_STARTED)

typedef void(h2o_nif_port_on_close_t)(ErlNifEnv *env, h2o_nif_port_t *port);

struct h2o_nif_port_s {
    h2o_linklist_t _link;
    ErlNifRWLock *rwlock;
    h2o_nif_port_t *parent;
    h2o_linklist_t children;
    int id;
    ErlNifPid owner;
    int state;
    int active;
    int16_t active_count;
    volatile sig_atomic_t opened;
    volatile sig_atomic_t closed;
    volatile sig_atomic_t released;
    volatile sig_atomic_t release_count;
    int type;
    h2o_nif_port_on_close_t *on_close;
    void *data;
    // h2o_nif_queue_t acc;
    // h2o_nif_queue_t req;
};

// extern ERL_NIF_TERM h2o_nif_port_mem_info(ErlNifEnv *env);
extern h2o_nif_port_t *h2o_nif_port_alloc(ErlNifEnv *env);
extern h2o_nif_port_t *h2o_nif_port_create(ErlNifEnv *env, h2o_nif_port_t *parent);
extern int h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp);
extern int h2o_nif_port_get_by_id(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp);
// extern void h2o_nif_port_release(h2o_nif_port_t *port);
extern int h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port);
// extern int h2o_nif_port_refc(h2o_nif_port_t *port);
extern int h2o_nif_port_release(h2o_nif_port_t *port);
extern void h2o_nif_port_dtor(ErlNifEnv *env, void *obj);
// extern int h2o_nif_port_checkout(ErlNifEnv *env, ERL_NIF_TERM port_id_term, h2o_nif_port_t **portp, int readwrite);
// extern void h2o_nif_port_checkin(h2o_nif_port_t **portp, int readwrite);
// extern ERL_NIF_TERM h2o_nif_port_open(ErlNifEnv *env, h2o_nif_port_t **newport);
// extern ERL_NIF_TERM h2o_nif_port_close(ErlNifEnv *env, ERL_NIF_TERM port_id_term);
// extern ERL_NIF_TERM h2o_nif_port_spawn(ErlNifEnv *env, h2o_nif_port_t *parent, h2o_nif_port_t **newport);

static ERL_NIF_TERM h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port);
static int h2o_nif_port_rlock(h2o_nif_port_t *port);
static int h2o_nif_port_runlock(h2o_nif_port_t *port);
static int h2o_nif_port_rwlock(h2o_nif_port_t *port);
static int h2o_nif_port_rwunlock(h2o_nif_port_t *port);
static int h2o_nif_port_tryclose(h2o_nif_port_t *port);
static int h2o_nif_port_tryrelease(h2o_nif_port_t *port);

static inline ERL_NIF_TERM
h2o_nif_port_make(ErlNifEnv *env, h2o_nif_port_t *port)
{
    if (port == NULL) {
        return enif_make_badarg(env);
    }
    if (__sync_bool_compare_and_swap(&port->opened, 0, 1)) {
        return enif_make_tuple3(env, ATOM_h2o_port, enif_make_int(env, port->id), enif_make_resource(env, (void *)port));
    } else {
        return enif_make_int(env, port->id);
    }
}

static inline int
h2o_nif_port_rlock(h2o_nif_port_t *port)
{
    if (port == NULL || port->rwlock == NULL) {
        return 0;
    }
    (void)enif_rwlock_rlock(port->rwlock);
    return 1;
}

static inline int
h2o_nif_port_runlock(h2o_nif_port_t *port)
{
    if (port == NULL || port->rwlock == NULL) {
        return 0;
    }
    (void)enif_rwlock_runlock(port->rwlock);
    return 1;
}

static inline int
h2o_nif_port_rwlock(h2o_nif_port_t *port)
{
    if (port == NULL || port->rwlock == NULL) {
        return 0;
    }
    (void)enif_rwlock_rwlock(port->rwlock);
    return 1;
}

static inline int
h2o_nif_port_rwunlock(h2o_nif_port_t *port)
{
    if (port == NULL || port->rwlock == NULL) {
        return 0;
    }
    (void)enif_rwlock_rwunlock(port->rwlock);
    return 1;
}

static inline int
h2o_nif_port_tryclose(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 0;
    }
    if (!__sync_bool_compare_and_swap(&port->closed, 0, 1)) {
        return 0;
    }
    if (!h2o_nif_port_rwlock(port)) {
        TRACE_F("Unable to obtain rwlock while closing port\n");
        return 0;
    }
    return 1;
}

static inline int
h2o_nif_port_tryrelease(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 0;
    }
    if (!__sync_bool_compare_and_swap(&port->released, 0, 1)) {
        return 0;
    }
    return 1;
}

#endif
