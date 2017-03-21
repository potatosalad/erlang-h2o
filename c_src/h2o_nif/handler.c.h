// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../handler.h"
#include "../ipc.h"
#include "../slice.h"

/* fun h2o_nif:handler_read_start/1 */

static ERL_NIF_TERM
h2o_nif_handler_read_start_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_handler_t *handler = NULL;
    if (argc != 1 || !h2o_nif_handler_get(env, argv[0], &handler)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&handler->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!atomic_flag_test_and_set_explicit(&handler->state, memory_order_relaxed)) {
        (void)atomic_flag_clear_explicit(&handler->state, memory_order_relaxed);
        return ATOM_ok;
    } else {
        ERL_NIF_TERM msg;
        msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, &handler->super), ATOM_ready_input);
        (void)h2o_nif_port_send(env, &handler->super, NULL, msg);
        return ATOM_ok;
    }
}

/* fun h2o_nif:handler_read/1 */

typedef struct h2o_nif_handler_read_1_s h2o_nif_handler_read_1_t;

struct h2o_nif_handler_read_1_s {
    size_t max_per_slice;
    size_t offset;
    size_t length;
    h2o_linklist_t events;
    h2o_linklist_t *node;
};

static ERL_NIF_TERM h2o_nif_handler_read_trap_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_handler_read_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_handler_t *handler = NULL;
    if (argc != 1 || !h2o_nif_handler_get(env, argv[0], &handler)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(&handler->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    h2o_linklist_t events;
    unsigned long num_events;
    (void)atomic_flag_clear_explicit(&handler->state, memory_order_relaxed);
    (void)h2o_linklist_init_anchor(&events);
    num_events = atomic_load_explicit(&handler->num_events, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&handler->spinlock);
    (void)h2o_linklist_insert_list(&events, &handler->events);
    (void)ck_spinlock_unlock(&handler->spinlock);
    if (h2o_linklist_is_empty(&events)) {
        return enif_make_list(env, 0);
    }

    // DEBUG_F("num_events: %lu\n", num_events);

    if (num_events <= MAX_PER_SLICE) {
        ERL_NIF_TERM list;
        list = enif_make_list(env, 0);
        h2o_linklist_t *anchor = &events;
        h2o_linklist_t *node = anchor->prev;
        h2o_nif_handler_event_t *event = NULL;
        unsigned long count = 0;
        while (node != anchor) {
            event = H2O_STRUCT_FROM_MEMBER(h2o_nif_handler_event_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_port_make(env, &event->super), list);
            node = node->prev;
            count++;
        }
        (void)atomic_fetch_sub_explicit(&handler->num_events, count, memory_order_relaxed);
        return list;
    }

    h2o_nif_handler_read_1_t *trap = enif_alloc_resource(h2o_nif_trap_resource_type, sizeof(*trap));
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
    newargv[0] = argv[0]; /* [0] handler */
    newargv[1] = enif_make_resource(env, (void *)trap); /* [1] trap */
    newargv[2] = enif_make_list(env, 0); /* [2] list */
    (void)enif_release_resource((void *)trap);

    return enif_schedule_nif(env, "handler_read", 0, h2o_nif_handler_read_trap_3, 3, newargv);
}

static ERL_NIF_TERM
h2o_nif_handler_read_trap_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_handler_t *handler = NULL;
    h2o_nif_handler_read_1_t *trap = NULL;
    if (argc != 3 || !h2o_nif_handler_get(env, argv[0], &handler) || !enif_get_resource(env, argv[1], h2o_nif_trap_resource_type, (void **)&trap) || !enif_is_list(env, argv[2])) {
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
    h2o_nif_handler_event_t *event = NULL;
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
            event = H2O_STRUCT_FROM_MEMBER(h2o_nif_handler_event_t, _link, node);
            list = enif_make_list_cell(env, h2o_nif_port_make(env, &event->super), list);
            node = node->prev;
            count++;
        }
        trap->node = node;
        (void)atomic_fetch_sub_explicit(&handler->num_events, count, memory_order_relaxed);
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
            newargv[0] = argv[0]; /* [0] handler */
            newargv[1] = argv[1]; /* [1] trap */
            newargv[2] = list; /* [2] list */
            return enif_schedule_nif(env, "handler_read", 0, h2o_nif_handler_read_trap_3, argc, newargv);
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

/* fun h2o_nif:handler_event_reply/4 */

static ERL_NIF_TERM h2o_nif_handler_event_reply_trap_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static void
__h2o_nif_handler_event_reply_4(h2o_nif_handler_event_t *event)
{
    TRACE_F("__h2o_nif_handler_event_reply_4:%s:%d\n", __FILE__, __LINE__);
    ErlNifEnv *env = event->finalizer.env;
    unsigned int status = event->finalizer.status;
    ERL_NIF_TERM headers = event->finalizer.headers;
    ErlNifBinary body = event->finalizer.body;
    h2o_req_t *req = event->req;

    req->res.status = status;
    {
        ERL_NIF_TERM key;
        ERL_NIF_TERM value;
        ErlNifMapIterator iter;
        ErlNifBinary name_bin;
        ErlNifBinary value_bin;
        assert(enif_map_iterator_create(env, headers, &iter, ERL_NIF_MAP_ITERATOR_FIRST));
        while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
            if (enif_inspect_iolist_as_binary(env, key, &name_bin) && enif_inspect_iolist_as_binary(env, value, &value_bin)) {
                (void)h2o_add_header_by_str(&req->pool, &req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL, (const char *)value_bin.data, value_bin.size);
            }
            (void)enif_map_iterator_next(env, &iter);
        }
        (void)enif_map_iterator_destroy(env, &iter);
    }
    (void)h2o_send_inline(req, (const char *)body.data, body.size);
    (void)h2o_nif_port_close_silent(&event->super, NULL, NULL);
    (void)atomic_fetch_sub_explicit(&event->num_async, 1, memory_order_relaxed);
    // (void)h2o_nif_port_release(&event->super);
}

static ERL_NIF_TERM
h2o_nif_handler_event_reply_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_4:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *event = NULL;
    if (argc != 4 || !h2o_nif_handler_event_get(env, argv[0], &event)) {
        return enif_make_badarg(env);
    }
    unsigned int status;
    ERL_NIF_TERM headers = argv[2];
    ErlNifBinary body;
    if (!enif_get_uint(env, argv[1], &status) || status < 100 || status > 599 ||
        !enif_is_map(env, headers) || !enif_inspect_iolist_as_binary(env, argv[3], &body)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_set_finalized(&event->super)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    (void)atomic_fetch_add_explicit(&event->num_async, 1, memory_order_relaxed);
    // (void)h2o_nif_port_keep(&event->super);
    event->finalizer.env = env;
    event->finalizer.status = status;
    event->finalizer.headers = headers;
    event->finalizer.body = body;
    (void)h2o_nif_ipc_request(event->req, (h2o_nif_ipc_callback_t *)__h2o_nif_handler_event_reply_4, (void *)event);
    (void)ck_pr_stall();
    if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
        return ATOM_ok;
        // ERL_NIF_TERM newargv[1];
        // newargv[0] = argv[0];
        // return h2o_nif_port_close_1(env, 1, newargv);
    }
    return enif_schedule_nif(env, "handler_event_reply", 0, h2o_nif_handler_event_reply_trap_4, argc, argv);
}

static ERL_NIF_TERM
h2o_nif_handler_event_reply_trap_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_trap_4:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_t *event = NULL;
    if (argc != 4 || !h2o_nif_handler_event_get(env, argv[0], &event)) {
        return enif_make_badarg(env);
    }
    if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
        return ATOM_ok;
        // ERL_NIF_TERM newargv[1];
        // newargv[0] = argv[0];
        // return h2o_nif_port_close_1(env, 1, newargv);
    }
    return enif_schedule_nif(env, "handler_event_reply", 0, h2o_nif_handler_event_reply_trap_4, argc, argv);
}

/* fun h2o_nif:handler_event_reply_batch/1 */

typedef struct h2o_nif_handler_event_reply_batch_1_s h2o_nif_handler_event_reply_batch_1_t;

struct h2o_nif_handler_event_reply_batch_1_s {
    ErlNifEnv *env;
    ERL_NIF_TERM list;
};

static ERL_NIF_TERM h2o_nif_handler_event_reply_batch_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_handler_event_reply_batch_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_batch_1:%s:%d\n", __FILE__, __LINE__);
    if (argc != 1 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }
    h2o_nif_handler_event_reply_batch_1_t *trap = enif_alloc_resource(h2o_nif_trap_resource_type, sizeof(*trap));
    if (trap == NULL) {
        return enif_make_badarg(env);
    }
    trap->env = enif_alloc_env();
    if (trap->env == NULL) {
        (void)enif_release_resource((void *)trap);
        return enif_make_badarg(env);
    }
    trap->list = enif_make_list(trap->env, 0);
    // trap->list = enif_make_copy(trap->env, argv[0]);
    h2o_nif_handler_event_t *event = NULL;
    unsigned int status;
    ERL_NIF_TERM headers;
    ErlNifBinary body;
    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    int arity;
    const ERL_NIF_TERM *array;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        list = tail;
        if (!enif_get_tuple(env, head, &arity, &array) || arity != 4) {
            continue;
        }
        if (!h2o_nif_handler_event_get(env, array[0], &event)) {
            continue;
        }
        headers = array[2];
        if (!enif_get_uint(env, array[1], &status) || status < 100 || status > 599 ||
            !enif_is_map(env, headers) || !enif_inspect_iolist_as_binary(env, array[3], &body)) {
            continue;
        }
        if (!h2o_nif_port_set_finalized(&event->super)) {
            continue;
        }
        assert(enif_inspect_iolist_as_binary(trap->env, enif_make_copy(trap->env, array[3]), &body));
        (void)atomic_fetch_add_explicit(&event->num_async, 1, memory_order_relaxed);
        event->finalizer.env = trap->env;
        event->finalizer.status = status;
        event->finalizer.headers = enif_make_copy(trap->env, headers);
        event->finalizer.body = body;
        trap->list = enif_make_list_cell(trap->env, enif_make_copy(trap->env, array[0]), trap->list);
        // (void)h2o_nif_ipc_request(event->req, (h2o_nif_ipc_callback_t *)__h2o_nif_handler_event_reply_4, (void *)event);
    }
    list = trap->list;
    while (enif_get_list_cell(trap->env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(trap->env, head, &event)) {
            continue;
        }
        (void)h2o_nif_ipc_request(event->req, (h2o_nif_ipc_callback_t *)__h2o_nif_handler_event_reply_4, (void *)event);
    }
    (void)ck_pr_stall();
    int done = 1;
    list = trap->list;
    while (enif_get_list_cell(trap->env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(trap->env, head, &event)) {
            continue;
        }
        if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
            continue;
        }
        done = 0;
        break;
    }
    if (done) {
        (void)enif_free_env(trap->env);
        (void)enif_release_resource((void *)trap);
        return ATOM_ok;
    }

    ERL_NIF_TERM newargv[1];
    newargv[0] = enif_make_resource(env, (void *)trap); /* [0] trap */
    (void)enif_release_resource((void *)trap);

    return enif_schedule_nif(env, "handler_event_reply_batch", 0, h2o_nif_handler_event_reply_batch_trap_1, 1, newargv);
}

static ERL_NIF_TERM
h2o_nif_handler_event_reply_batch_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_batch_trap_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_event_reply_batch_1_t *trap = NULL;
    if (argc != 1 || !enif_get_resource(env, argv[0], h2o_nif_trap_resource_type, (void **)&trap)) {
        return enif_make_badarg(env);
    }
    h2o_nif_handler_event_t *event = NULL;
    ERL_NIF_TERM list = trap->list;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    int done = 1;
    while (enif_get_list_cell(trap->env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(trap->env, head, &event)) {
            continue;
        }
        if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
            continue;
        }
        done = 0;
        break;
    }
    if (done) {
        (void)enif_free_env(trap->env);
        return ATOM_ok;
    }
    return enif_schedule_nif(env, "handler_event_reply_batch", 0, h2o_nif_handler_event_reply_batch_trap_1, argc, argv);
}

/* fun h2o_nif:handler_event_reply_multi/4 */

static ERL_NIF_TERM h2o_nif_handler_event_reply_multi_trap_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_handler_event_reply_multi_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_multi_4:%s:%d\n", __FILE__, __LINE__);
    if (argc != 4 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }
    unsigned int status;
    ERL_NIF_TERM headers = argv[2];
    ErlNifBinary body;
    if (!enif_get_uint(env, argv[1], &status) || status < 100 || status > 599 ||
        !enif_is_map(env, headers) || !enif_inspect_iolist_as_binary(env, argv[3], &body)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    h2o_nif_handler_event_t *event = NULL;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(env, head, &event)) {
            continue;
        }
        if (!h2o_nif_port_set_finalized(&event->super)) {
            continue;
        }
        (void)atomic_fetch_add_explicit(&event->num_async, 1, memory_order_relaxed);
        event->finalizer.env = env;
        event->finalizer.status = status;
        event->finalizer.headers = headers;
        event->finalizer.body = body;
        (void)h2o_nif_ipc_request(event->req, (h2o_nif_ipc_callback_t *)__h2o_nif_handler_event_reply_4, (void *)event);
    }
    (void)ck_pr_stall();
    int done = 1;
    list = argv[0];
    while (enif_get_list_cell(env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(env, head, &event)) {
            continue;
        }
        if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
            continue;
        }
        done = 0;
        break;
    }
    if (done) {
        return ATOM_ok;
    }
    return enif_schedule_nif(env, "handler_event_reply_multi", 0, h2o_nif_handler_event_reply_multi_trap_4, argc, argv);
}

static ERL_NIF_TERM
h2o_nif_handler_event_reply_multi_trap_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_handler_event_reply_multi_trap_4:%s:%d\n", __FILE__, __LINE__);
    if (argc != 4 || !enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    h2o_nif_handler_event_t *event = NULL;
    int done = 1;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        list = tail;
        if (!h2o_nif_handler_event_get(env, head, &event)) {
            continue;
        }
        if (atomic_load_explicit(&event->num_async, memory_order_relaxed) == 0) {
            continue;
        }
        done = 0;
        break;
    }
    if (done) {
        return ATOM_ok;
    }
    return enif_schedule_nif(env, "handler_event_reply_multi", 0, h2o_nif_handler_event_reply_multi_trap_4, argc, argv);
}
