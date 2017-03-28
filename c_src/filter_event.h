// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_FILTER_EVENT_H
#define H2O_NIF_FILTER_EVENT_H

#include "globals.h"
#include "port.h"
#include "filter.h"

/* Types */

typedef struct h2o_nif_filter_event_s h2o_nif_filter_event_t;
typedef struct h2o_nif_filter_input_s h2o_nif_filter_input_t;
typedef struct h2o_nif_filter_output_s h2o_nif_filter_output_t;
typedef struct h2o_nif_filter_ostream_s h2o_nif_filter_ostream_t;

struct h2o_nif_filter_event_s {
    h2o_nif_port_t super;
    h2o_linklist_t _link;
    h2o_req_t *req;
    _Atomic size_t entity_offset;
    struct {
        ck_spinlock_t lock;
        _Atomic int skip;
        h2o_linklist_t input;
        _Atomic size_t num_input;
        atomic_flag ready_input;
        h2o_send_state_t input_state;
        h2o_linklist_t output;
        _Atomic size_t num_output;
        atomic_flag ready_output;
        _Atomic h2o_send_state_t output_state;
        _Atomic uintptr_t output_batch;
        h2o_timeout_entry_t output_timeout;
    } state;
    _Atomic uintptr_t ostream;
};

struct h2o_nif_filter_input_s {
    h2o_linklist_t _link;
    ErlNifBinary binary;
};

struct h2o_nif_filter_output_s {
    h2o_linklist_t _link;
    h2o_send_state_t state;
    h2o_iovec_t iov;
};

struct h2o_nif_filter_ostream_s {
    h2o_ostream_t super;
    _Atomic uintptr_t event;
};

/* Resource Functions */

static int h2o_nif_filter_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_event_t **eventp);
extern ERL_NIF_TERM h2o_nif_filter_event_make(ErlNifEnv *env, h2o_nif_filter_event_t *event);

inline int
h2o_nif_filter_event_get(ErlNifEnv *env, ERL_NIF_TERM port_term, h2o_nif_filter_event_t **eventp)
{
    assert(eventp != NULL);
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, port_term, &port) || port->type != H2O_NIF_PORT_TYPE_FILTER_EVENT) {
        *eventp = NULL;
        return 0;
    }
    *eventp = (h2o_nif_filter_event_t *)port;
    return 1;
}

/* Port Functions */

extern int h2o_nif_filter_event_open(h2o_nif_filter_t *filter, h2o_req_t *req, h2o_ostream_t **slot,
                                     h2o_nif_filter_event_t **eventp);

#endif
