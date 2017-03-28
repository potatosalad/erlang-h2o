// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../filter.h"
#include "../filter_event.h"
#include "../ipc.h"
#include "../slice.h"

/* fun h2o_nif:filter_read_start/1 */

static ERL_NIF_TERM
h2o_nif_filter_read_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_filter_t *filter = NULL;
    if (argc != 1 || !h2o_nif_filter_get(env, argv[0], &filter)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&filter->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!atomic_flag_test_and_set_explicit(&filter->state, memory_order_relaxed)) {
        (void)atomic_flag_clear_explicit(&filter->state, memory_order_relaxed);
        return ATOM_ok;
    } else {
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &filter->super), ATOM_ready_input);
        (void)h2o_nif_port_send(env, &filter->super, NULL, msg);
        return ATOM_ok;
    }
}

/* fun h2o_nif:filter_read/1 */

static ERL_NIF_TERM h2o_nif_filter_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list,
                                                 h2o_linklist_t *node);

static ERL_NIF_TERM
h2o_nif_filter_read_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_filter_t *filter = NULL;
    if (argc != 1 || !h2o_nif_filter_get(env, argv[0], &filter)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&filter->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    h2o_linklist_t events;
    unsigned long num_events;
    (void)atomic_flag_clear_explicit(&filter->state, memory_order_relaxed);
    (void)h2o_linklist_init_anchor(&events);
    num_events = atomic_load_explicit(&filter->num_events, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&filter->spinlock);
    (void)h2o_linklist_insert_list(&events, &filter->events);
    (void)ck_spinlock_unlock(&filter->spinlock);
    if (h2o_linklist_is_empty(&events)) {
        return enif_make_list(env, 0);
    }

    if (num_events <= MAX_PER_SLICE) {
        ERL_NIF_TERM list;
        list = enif_make_list(env, 0);
        h2o_linklist_t *anchor = &events;
        h2o_linklist_t *node = anchor->prev;
        h2o_nif_filter_event_t *event = NULL;
        unsigned long count = 0;
        while (node != anchor) {
            event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_filter_event_make(env, event), list);
            node = node->prev;
            count++;
        }
        (void)atomic_fetch_sub_explicit(&filter->num_events, count, memory_order_relaxed);
        return list;
    }

    h2o_nif_slicelist_t *slicelist = NULL;
    if (!h2o_nif_slicelist_create("filter_read", h2o_nif_filter_read_1_reduce, &slicelist)) {
        return enif_make_badarg(env);
    }
    slicelist->length = num_events;
    (void)h2o_linklist_insert_list(&slicelist->list, &events);
    (void)h2o_nif_port_keep(&filter->super);
    slicelist->data = (void *)filter;

    return h2o_nif_slicelist_schedule(env, slicelist);
}

static ERL_NIF_TERM
h2o_nif_filter_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list, h2o_linklist_t *node)
{
    /* final reduction */
    if (node == NULL) {
        h2o_nif_filter_t *filter = (void *)slicelist->data;
        (void)atomic_fetch_sub_explicit(&filter->num_events, slicelist->count, memory_order_relaxed);
        (void)h2o_nif_port_release(&filter->super);
        return list;
    }

    /* intermediate reduction */
    h2o_nif_filter_event_t *event = NULL;
    event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, _link, node);
    list = enif_make_list_cell(env, h2o_nif_filter_event_make(env, event), list);
    return list;
}
