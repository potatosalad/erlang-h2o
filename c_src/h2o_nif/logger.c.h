// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../logger.h"
#include "../slice.h"

/* fun h2o_nif:logger_read_start/1 */

static ERL_NIF_TERM
h2o_nif_logger_read_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_logger_t *logger = NULL;
    if (argc != 1 || !h2o_nif_logger_get(env, argv[0], &logger)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&logger->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!atomic_flag_test_and_set_explicit(&logger->events.ready_input, memory_order_relaxed)) {
        (void)atomic_flag_clear_explicit(&logger->events.ready_input, memory_order_relaxed);
        return ATOM_ok;
    } else {
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &logger->super), ATOM_ready_input);
        (void)h2o_nif_port_send(env, &logger->super, NULL, msg);
        return ATOM_ok;
    }
}

/* fun h2o_nif:logger_read/1 */

static ERL_NIF_TERM h2o_nif_logger_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list,
                                                 h2o_linklist_t *node);

static ERL_NIF_TERM
h2o_nif_logger_read_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_logger_t *logger = NULL;
    if (argc != 1 || !h2o_nif_logger_get(env, argv[0], &logger)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&logger->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    h2o_linklist_t events;
    size_t num_events;
    (void)h2o_linklist_init_anchor(&events);
    (void)atomic_flag_clear_explicit(&logger->events.ready_input, memory_order_relaxed);
    num_events = atomic_load_explicit(&logger->events.size, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&logger->events.lock);
    (void)h2o_linklist_insert_list(&events, &logger->events.input);
    (void)ck_spinlock_unlock(&logger->events.lock);
    if (h2o_linklist_is_empty(&events)) {
        return enif_make_list(env, 0);
    }

    if (num_events <= MAX_PER_SLICE) {
        ERL_NIF_TERM list;
        list = enif_make_list(env, 0);
        h2o_linklist_t *anchor = &events;
        h2o_linklist_t *node = anchor->prev;
        h2o_nif_logger_event_t *logger_event = NULL;
        size_t count = 0;
        while (node != anchor) {
            logger_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_event_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_logger_event_make(env, logger_event), list);
            node = node->prev;
            count++;
        }
        while (!h2o_linklist_is_empty(&events)) {
            logger_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_event_t, _link, events.next);
            (void)h2o_linklist_unlink(&logger_event->_link);
            (void)mem_free(logger_event);
        }
        (void)atomic_fetch_sub_explicit(&logger->events.size, count, memory_order_relaxed);
        return list;
    }

    h2o_nif_slicelist_t *slicelist = NULL;
    if (!h2o_nif_slicelist_create("logger_read", h2o_nif_logger_read_1_reduce, &slicelist)) {
        return enif_make_badarg(env);
    }
    slicelist->length = num_events;
    (void)h2o_linklist_insert_list(&slicelist->list, &events);
    (void)h2o_nif_port_keep(&logger->super);
    slicelist->data = (void *)logger;

    return h2o_nif_slicelist_schedule(env, slicelist);
}

static ERL_NIF_TERM
h2o_nif_logger_read_1_reduce(ErlNifEnv *env, h2o_nif_slicelist_t *slicelist, ERL_NIF_TERM list, h2o_linklist_t *node)
{
    /* final reduction */
    if (node == NULL) {
        h2o_nif_logger_t *logger = (void *)slicelist->data;
        h2o_nif_logger_event_t *logger_event = NULL;
        while (!h2o_linklist_is_empty(&slicelist->list)) {
            logger_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_event_t, _link, slicelist->list.next);
            (void)h2o_linklist_unlink(&logger_event->_link);
            (void)mem_free(logger_event);
        }
        (void)atomic_fetch_sub_explicit(&logger->events.size, slicelist->count, memory_order_relaxed);
        (void)h2o_nif_port_release(&logger->super);
        return list;
    }

    /* intermediate reduction */
    h2o_nif_logger_event_t *logger_event = NULL;
    logger_event = H2O_STRUCT_FROM_MEMBER(h2o_nif_logger_event_t, _link, node);
    list = enif_make_list_cell(env, h2o_nif_logger_event_make(env, logger_event), list);
    return list;
}
