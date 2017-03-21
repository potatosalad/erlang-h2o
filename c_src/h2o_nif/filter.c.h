// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../filter.h"
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

typedef struct h2o_nif_filter_read_1_s h2o_nif_filter_read_1_t;

struct h2o_nif_filter_read_1_s {
    size_t max_per_slice;
    size_t offset;
    size_t length;
    h2o_linklist_t events;
    h2o_linklist_t *node;
};

static ERL_NIF_TERM h2o_nif_filter_read_trap_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

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
            list = enif_make_list_cell(env, h2o_nif_port_make(env, &event->super), list);
            node = node->prev;
            count++;
        }
        (void)atomic_fetch_sub_explicit(&filter->num_events, count, memory_order_relaxed);
        return list;
    }

    h2o_nif_filter_read_1_t *trap = enif_alloc_resource(h2o_nif_trap_resource_type, sizeof(*trap));
    if (trap == NULL) {
        return enif_make_badarg(env);
    }
    (void)memset(trap, 0, sizeof(*trap));

    trap->max_per_slice = MAX_PER_SLICE;
    trap->offset = 0;
    trap->length = num_events;
    (void)h2o_linklist_init_anchor(&trap->events);
    (void)h2o_linklist_insert_list(&trap->events, &events);
    trap->node = trap->events.prev;

    ERL_NIF_TERM newargv[3];
    newargv[0] = argv[0]; /* [0] filter */
    newargv[1] = enif_make_resource(env, (void *)trap); /* [1] trap */
    newargv[2] = enif_make_list(env, 0); /* [2] list */
    (void)enif_release_resource((void *)trap);

    return enif_schedule_nif(env, "filter_read", 0, h2o_nif_filter_read_trap_3, 3, newargv);
}

static ERL_NIF_TERM
h2o_nif_filter_read_trap_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_filter_t *filter = NULL;
    h2o_nif_filter_read_1_t *trap = NULL;
    if (argc != 3 || !h2o_nif_filter_get(env, argv[0], &filter) || !enif_get_resource(env, argv[1], h2o_nif_trap_resource_type, (void **)&trap) || !enif_is_list(env, argv[2])) {
        return enif_make_badarg(env);
    }

    struct timeval start;
    struct timeval stop;
    struct timeval timeslice;
    size_t end;
    size_t i;
    int percent;
    int total;
    size_t max_per_slice;
    size_t offset;

    total = 0;
    max_per_slice = trap->max_per_slice;
    offset = trap->offset;

    end = offset + max_per_slice;

    if (end > trap->length) {
        end = trap->length;
    }

    i = offset;

    ERL_NIF_TERM list;
    h2o_linklist_t *anchor = NULL;
    h2o_linklist_t *node = NULL;
    h2o_nif_filter_event_t *event = NULL;
    size_t count;
    size_t slice;

    list = argv[2];
    anchor = &trap->events;
    node = trap->node;
    count = 0;

    while (i < trap->length) {
        (void)gettimeofday(&start, NULL);
        slice = end - i;
        while (node != anchor && (count < slice)) {
            event = H2O_STRUCT_FROM_MEMBER(h2o_nif_filter_event_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_port_make(env, &event->super), list);
            node = node->prev;
            count++;
        }
        trap->node = node;
        (void)atomic_fetch_sub_explicit(&filter->num_events, count, memory_order_relaxed);
        if (node == anchor) {
            i = trap->length;
        } else {
            i = (offset + count);
        }
        if (i == trap->length) {
            break;
        }
        (void)gettimeofday(&stop, NULL);
        /* determine how much of the timeslice was used */
        timersub(&stop, &start, &timeslice);
        percent = (int)((timeslice.tv_sec * 1000000 + timeslice.tv_usec) / 10);
        total += percent;
        if (percent > 100) {
            percent = 100;
        } else if (percent == 0) {
            percent = 1;
        }
        if (enif_consume_timeslice(env, percent)) {
            /* the timeslice has been used up, so adjust our max_per_slice byte count based on the processing we've done, then
             * reschedule to run again */
            max_per_slice = i - offset;
            if (total > 100) {
                int m = (int)(total / 100);
                if (m == 1) {
                    max_per_slice -= (unsigned long)(max_per_slice * (total - 100) / 100);
                } else {
                    max_per_slice = (unsigned long)(max_per_slice / m);
                }
            }
            trap->max_per_slice = max_per_slice;
            trap->offset = i;
            ERL_NIF_TERM newargv[3];
            newargv[0] = argv[0]; /* [0] filter */
            newargv[1] = argv[1]; /* [1] trap */
            newargv[2] = list; /* [2] list */
            return enif_schedule_nif(env, "filter_read", 0, h2o_nif_filter_read_trap_3, argc, newargv);
        }
        end += max_per_slice;
        if (end > trap->length) {
            end = trap->length;
        }
    }

    trap->max_per_slice = max_per_slice;
    trap->offset = i;

    return list;
}

/* fun h2o_nif:filter_event_setup_next_ostream/1 */

static void
__h2o_nif_filter_event_setup_next_ostream_1(h2o_nif_filter_event_t *event)
{
    (void)h2o_setup_next_ostream(event->req, event->slot);
    (void)h2o_nif_port_release(&event->super);
}

static ERL_NIF_TERM
h2o_nif_filter_event_setup_next_ostream_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_filter_event_t *event = NULL;
    if (argc != 1 || !h2o_nif_filter_event_get(env, argv[0], &event)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_set_finalized(&event->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    (void)h2o_nif_port_keep(&event->super);
    (void)h2o_nif_ipc_request(event->req, (h2o_nif_ipc_callback_t *)__h2o_nif_filter_event_setup_next_ostream_1, (void *)event);
    return ATOM_ok;
}
