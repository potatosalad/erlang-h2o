// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_FILTER_EVENT_H
#define H2O_NIF_FILTER_EVENT_H

#include "globals.h"
#include "port.h"
#include "filter.h"
#include "ipc.h"
#include "records.h"

/* Types */

typedef struct h2o_nif_filter_event_s h2o_nif_filter_event_t;
typedef struct h2o_nif_filter_event_ientry_s h2o_nif_filter_event_ientry_t;
typedef struct h2o_nif_filter_event_oentry_s h2o_nif_filter_event_oentry_t;

struct h2o_nif_filter_event_ientry_s {
    h2o_linklist_t _link;
    ErlNifBinary binary;
};

struct h2o_nif_filter_event_oentry_s {
    h2o_linklist_t _link;
    h2o_iovec_vector_t data;
    // h2o_iovec_t *bufs;
    // size_t bufcnt;
    h2o_send_state_t state;
};

struct h2o_nif_filter_event_s {
    h2o_nif_port_t super;
    h2o_linklist_t _link;
    h2o_nif_ipc_ostream_t ostream;
    h2o_nif_ipc_request_t request;
    struct {
        ck_spinlock_t lock;
        h2o_linklist_t queue;
        size_t size;
        h2o_send_state_t state;
        atomic_flag flag;
        h2o_timeout_entry_t timeout_entry;
    } rx;
    struct {
        ck_spinlock_t lock;
        h2o_linklist_t queue;
        size_t size;
        h2o_send_state_t state;
        atomic_flag flag;
        h2o_nif_ipc_message_t message;
        h2o_timeout_entry_t timeout_entry;
    } tx;
    h2o_nif_proto_req_t proto;
};

/* Resource Functions */

static int h2o_nif_filter_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_event_t **filter_eventp);
extern ERL_NIF_TERM h2o_nif_filter_event_make(ErlNifEnv *env, h2o_nif_filter_event_t *filter_event);
extern ERL_NIF_TERM h2o_nif_filter_event_ientry_make(ErlNifEnv *env, h2o_nif_filter_event_ientry_t *fi);

inline int
h2o_nif_filter_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_event_t **filter_eventp)
{
    assert(filter_eventp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_FILTER_EVENT) {
        *filter_eventp = NULL;
        return 0;
    }
    *filter_eventp = (h2o_nif_filter_event_t *)port;
    return 1;
}

/* Port Functions */

extern int h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot,
                                     h2o_nif_filter_event_t **filter_eventp);

/* Filter Event Functions */

extern int h2o_nif_filter_event_send(h2o_nif_filter_event_t *filter_event, h2o_iovec_t *bufs, size_t bufcnt,
                                     h2o_send_state_t state);

#endif
