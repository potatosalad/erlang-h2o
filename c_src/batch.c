// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "batch.h"

ErlNifResourceType *h2o_nif_batch_resource_type = NULL;

static void h2o_nif_batch_dtor(ErlNifEnv *env, void *obj);
static int h2o_nif_batch_match_call(h2o_nif_batch_t *batch, ErlNifEnv *env, ErlNifPid to, ERL_NIF_TERM tag, ERL_NIF_TERM request);
static int h2o_nif_batch_match_cast(h2o_nif_batch_t *batch, ErlNifEnv *env, ERL_NIF_TERM request);
static void h2o_nif_batch_add_call(h2o_nif_batch_t *batch, h2o_nif_batch_fun_t *fun, ErlNifPid to, ERL_NIF_TERM tag, int argc,
                                   const ERL_NIF_TERM argv[]);
static void h2o_nif_batch_add_cast(h2o_nif_batch_t *batch, h2o_nif_batch_fun_t *fun, int argc, const ERL_NIF_TERM argv[]);
static h2o_nif_batch_req_t *h2o_nif_batch_reserve_req(h2o_nif_batch_t *batch);

#include "batch/filter_event.c.h"
// #include "batch/handler.c.h"
#include "batch/port.c.h"

/* Types */

typedef struct h2o_nif_batch_call_s h2o_nif_batch_call_t;

struct h2o_nif_batch_call_s {
    int arity;
    h2o_nif_batch_fun_t *fun;
};

typedef struct h2o_nif_batch_call_s h2o_nif_batch_cast_t;

struct h2o_nif_batch_cast_s {
    int arity;
    h2o_nif_batch_fun_t *fun;
};

static h2o_nif_batch_call_t h2o_nif_batch_call[] = {
    // /* 0 */ {1, &filter_event_read_1},
};
static size_t h2o_nif_batch_call_max = sizeof(h2o_nif_batch_call) / sizeof(h2o_nif_batch_call_t);

static h2o_nif_batch_cast_t h2o_nif_batch_cast[] = {
    /* 0 */ {2, &port_connect_2},
    // /* 1 */ {3, &filter_event_read_entity_3},
    {0, NULL},
    /* 2 */ {3, &filter_event_send_3},
};
static size_t h2o_nif_batch_cast_max = sizeof(h2o_nif_batch_cast) / sizeof(h2o_nif_batch_cast_t);

/* NIF Functions */

int
h2o_nif_batch_load(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    h2o_nif_batch_resource_type =
        enif_open_resource_type(env, NULL, "h2o_nif_batch", h2o_nif_batch_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return 0;
}

int
h2o_nif_batch_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

void
h2o_nif_batch_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    return;
}

/* Resource Functions */

int
h2o_nif_batch_create(ErlNifEnv *env, ERL_NIF_TERM list, h2o_nif_batch_t **batchp)
{
    TRACE_F("h2o_nif_batch_create:%s:%d\n", __FILE__, __LINE__);
    assert(batchp != NULL);
    unsigned length;
    if (!enif_get_list_length(env, list, (unsigned *)&length)) {
        *batchp = NULL;
        return 0;
    }
    size_t size = sizeof(h2o_nif_batch_t) + (sizeof(h2o_nif_batch_req_t) * length);
    // TRACE_F("h2o_nif_batch_create:%s:%d LENGTH=%lu, SIZE=%lu\n", __FILE__, __LINE__, length, size);
    h2o_nif_batch_t *batch = (h2o_nif_batch_t *)enif_alloc_resource(h2o_nif_batch_resource_type, size);
    if (batch == NULL) {
        *batchp = NULL;
        return 0;
    }
    (void)memset(batch, 0, size);
    batch->max_per_slice = MAX_PER_SLICE;
    batch->length = (size_t)length;
    batch->offset = 0;
    batch->num_req = 0;
    batch->req = NULL;
    // batch->req = ((void *)batch) + sizeof(h2o_nif_batch_req_t);
    // batch->req->next = NULL;
    batch->env = enif_alloc_env();
    if (batch->env == NULL) {
        (void)h2o_nif_batch_release(batch);
        *batchp = NULL;
        return 0;
    }
    // (void)h2o_linklist_init_anchor(&batch->queue[0]);
    // (void)h2o_linklist_init_anchor(&batch->queue[1]);
    // batch->node = NULL;
    *batchp = batch;
    return 1;
}

static void
h2o_nif_batch_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_batch_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_batch_t *batch = (h2o_nif_batch_t *)obj;
    if (batch->env != NULL) {
        (void)enif_free_env(batch->env);
        batch->env = NULL;
    }
    return;
}

/* Batch Functions */

ERL_NIF_TERM
h2o_nif_batch_process(h2o_nif_batch_t *batch, ErlNifEnv *env, ERL_NIF_TERM *listp)
{
    TRACE_F("h2o_nif_batch_process:%s:%d\n", __FILE__, __LINE__);
    if (!enif_is_list(env, *listp)) {
        return enif_make_badarg(env);
    }
    int arity;
    const ERL_NIF_TERM *array;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    ERL_NIF_TERM from;
    ERL_NIF_TERM request;
    ErlNifPid to;
    list = *listp;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        list = tail;
        if (!enif_get_tuple(env, head, &arity, &array)) {
            continue;
        } else if (arity == 2 && array[0] == ATOM_$gen_cast) {
            if (!h2o_nif_batch_match_cast(batch, env, array[1])) {
                continue;
            }
        } else if (arity == 3 && array[0] == ATOM_$gen_call) {
            from = array[1];
            request = array[2];
            if (!enif_get_tuple(env, from, &arity, &array) || arity != 2 || !enif_get_local_pid(env, array[0], &to) ||
                !enif_is_ref(env, array[1])) {
                TRACE_F("bad from field\n");
                continue;
            }
            if (!h2o_nif_batch_match_call(batch, env, to, array[1], request)) {
                TRACE_F("bad call match\n");
                continue;
            }
        }
    }
    *listp = list;
    return h2o_nif_batch_execute(batch, env);
}

ERL_NIF_TERM
h2o_nif_batch_execute(h2o_nif_batch_t *batch, ErlNifEnv *env)
{
    TRACE_F("h2o_nif_batch_execute:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_batch_req_t *req = batch->req;
    h2o_nif_batch_req_t *ptr = NULL;
    h2o_nif_batch_ctx_t ctx = {.batch = batch, .req = NULL};
    while (req != NULL) {
        ctx.req = req;
        if (!req->fun->exec(&ctx, env, req->argc, req->argv) || req->fun->done == NULL) {
            if (batch->req == req) {
                batch->req = req->next;
            } else {
                ptr = batch->req;
                while (ptr->next != req) {
                    ptr = ptr->next;
                }
                ptr->next = req->next;
            }
            batch->num_req--;
        }
        req = req->next;
    }
    return h2o_nif_batch_resolve(batch, env);
}

ERL_NIF_TERM
h2o_nif_batch_resolve(h2o_nif_batch_t *batch, ErlNifEnv *env)
{
    TRACE_F("h2o_nif_batch_resolve:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_batch_req_t *req = batch->req;
    h2o_nif_batch_req_t *ptr = NULL;
    h2o_nif_batch_ctx_t ctx = {.batch = batch, .req = NULL};
    while (req != NULL) {
        ctx.req = req;
        if (req->fun->done(&ctx, env, req->argc, req->argv)) {
            if (batch->req == req) {
                batch->req = req->next;
            } else {
                ptr = batch->req;
                while (ptr->next != req) {
                    ptr = ptr->next;
                }
                ptr->next = req->next;
            }
            batch->num_req--;
        }
        req = req->next;
    }
    if (batch->req == NULL) {
        return ATOM_ok;
    }
    return ATOM_finalize;
}

// struct a_s {
//     h2o_nif_batch_req_call_t call;
//     int a;
//     int b;
// };

// static int
// a_run(h2o_nif_batch_t *batch, ErlNifEnv *env, h2o_nif_batch_req_t *super)
// {
//     struct a_s *req = (struct a_s *)super;
//     (void)enif_send(env, &req->call.to, NULL, enif_make_tuple2(env, enif_make_copy(env, req->call.tag), enif_make_int(env, req->a
//     + req->b)));
//     (void)mem_free(req);
//     return 1;
// }

static int
h2o_nif_batch_match_call(h2o_nif_batch_t *batch, ErlNifEnv *env, ErlNifPid to, ERL_NIF_TERM tag, ERL_NIF_TERM request)
{
    TRACE_F("h2o_nif_batch_match_call:%s:%d\n", __FILE__, __LINE__);
    int arity;
    const ERL_NIF_TERM *array;
    unsigned int index;
    h2o_nif_batch_ctx_t ctx = {.batch = batch, .req = NULL};
    if (!enif_get_tuple(env, request, &arity, &array) || arity != 2 || !enif_get_uint(env, array[0], &index) ||
        index >= h2o_nif_batch_call_max || !enif_get_tuple(env, array[1], &arity, &array)) {
        return 0;
    }
    h2o_nif_batch_call_t call = h2o_nif_batch_call[index];
    if (call.arity != arity) {
        return 0;
    }
    if (call.fun->test == NULL || call.fun->test(&ctx, env, arity, array)) {
        (void)h2o_nif_batch_add_call(batch, call.fun, to, tag, arity, array);
        return 1;
    }
    return 0;
}

static int
h2o_nif_batch_match_cast(h2o_nif_batch_t *batch, ErlNifEnv *env, ERL_NIF_TERM request)
{
    TRACE_F("h2o_nif_batch_match_cast:%s:%d\n", __FILE__, __LINE__);
    int arity;
    const ERL_NIF_TERM *array;
    unsigned int index;
    h2o_nif_batch_ctx_t ctx = {.batch = batch, .req = NULL};
    if (!enif_get_tuple(env, request, &arity, &array) || arity != 2 || !enif_get_uint(env, array[0], &index) ||
        index >= h2o_nif_batch_cast_max || !enif_get_tuple(env, array[1], &arity, &array)) {
        return 0;
    }
    h2o_nif_batch_cast_t cast = h2o_nif_batch_cast[index];
    if (cast.arity != arity) {
        return 0;
    }
    if (cast.fun->test == NULL || cast.fun->test(&ctx, env, arity, array)) {
        (void)h2o_nif_batch_add_cast(batch, cast.fun, arity, array);
        return 1;
    }
    return 0;

    // if (arity == 3) {
    //     /* port_connect/2 */
    //     if (array[0] == ATOM_port_connect && h2o_nif_port_get(env, array[1], &port) && enif_get_local_pid(env, array[2], &pid)) {
    //         ERL_NIF_TERM newargv[2];
    //         newargv[0] = array[1];
    //         newargv[1] = array[2];
    //         (void)h2o_nif_batch_add_cast(batch, &port_connect_2, 2, newargv);
    //         return 1;
    //     }
    // } else if (arity == 4) {
    //     /* filter_event_read_entity/3 */
    //     if (array[0] == ATOM_filter_event_read_entity && h2o_nif_filter_event_get(env, array[1], &))
    //     /* handler_event_stream_reply/3 */
    //     if (array[0] == ATOM_handler_event_stream_reply && h2o_nif_handler_event_get(env, array[1], &handler_event)
    //         && (enif_get_uint(env, array[2], &status) && status >= 100 && status < 600)
    //         && enif_is_map(env, array[3])) {
    //         ERL_NIF_TERM newargv[3];
    //         newargv[0] = array[1];
    //         newargv[1] = array[2];
    //         newargv[2] = array[3];
    //         (void)h2o_nif_batch_add_cast(batch, &handler_event_stream_reply_3, 3, newargv);
    //         return 1;
    //     }
    //     /* handler_event_stream_body/3 */
    //     if (array[0] == ATOM_handler_event_stream_body && h2o_nif_handler_event_get(env, array[1], &handler_event)
    //         && (array[2] == ATOM_fin || array[2] == ATOM_nofin)
    //         && enif_inspect_iolist_as_binary(env, array[3], &body)) {
    //         ERL_NIF_TERM newargv[3];
    //         newargv[0] = array[1];
    //         newargv[1] = array[2];
    //         newargv[2] = array[3];
    //         (void)h2o_nif_batch_add_cast(batch, &handler_event_stream_body_3, 3, newargv);
    //         return 1;
    //     }
    //     TRACE_F("no match 4\n");
    // } else if (arity == 5) {
    //     /* handler_event_reply/4 */
    //     if (array[0] == ATOM_handler_event_reply && h2o_nif_handler_event_get(env, array[1], &handler_event)
    //         && (enif_get_uint(env, array[2], &status) && status >= 100 && status < 600)
    //         && enif_is_map(env, array[3]) && enif_inspect_iolist_as_binary(env, array[4], &body)) {
    //         ERL_NIF_TERM newargv[4];
    //         newargv[0] = array[1];
    //         newargv[1] = array[2];
    //         newargv[2] = array[3];
    //         newargv[3] = array[4];
    //         (void)h2o_nif_batch_add_cast(batch, &handler_event_reply_4, 4, newargv);
    //         return 1;
    //     }
    //     /* handler_event_read_body/4 */
    //     if (array[0] == ATOM_handler_event_read_body && h2o_nif_handler_event_get(env, array[1], &handler_event) &&
    //     enif_get_local_pid(env, array[2], &pid)
    //         && enif_get_ulong(env, array[3], &length)) {
    //         ERL_NIF_TERM newargv[3];
    //         newargv[0] = array[1];
    //         newargv[1] = array[2];
    //         newargv[2] = array[3];
    //         (void)h2o_nif_batch_add_cast(batch, &handler_event_read_body_4, 3, newargv);
    //         return 1;
    //     }
    // }
    // return 0;
}

static void
h2o_nif_batch_add_call(h2o_nif_batch_t *batch, h2o_nif_batch_fun_t *fun, ErlNifPid to, ERL_NIF_TERM tag, int argc,
                       const ERL_NIF_TERM argv[])
{
    assert(argc <= H2O_NIF_BATCH_MAX_ARITY);
    h2o_nif_batch_req_t *req = h2o_nif_batch_reserve_req(batch);
    req->fun = fun;
    req->from.to = to;
    req->from.tag = enif_make_copy(batch->env, tag);
    req->argc = argc;
    while ((argc--) != 0) {
        req->argv[argc] = enif_make_copy(batch->env, argv[argc]);
    }
    return;
}

static void
h2o_nif_batch_add_cast(h2o_nif_batch_t *batch, h2o_nif_batch_fun_t *fun, int argc, const ERL_NIF_TERM argv[])
{
    assert(argc <= H2O_NIF_BATCH_MAX_ARITY);
    h2o_nif_batch_req_t *req = h2o_nif_batch_reserve_req(batch);
    req->fun = fun;
    req->argc = argc;
    while ((argc--) != 0) {
        req->argv[argc] = enif_make_copy(batch->env, argv[argc]);
    }
    return;
}

static h2o_nif_batch_req_t *
h2o_nif_batch_reserve_req(h2o_nif_batch_t *batch)
{
    assert((++batch->num_req) <= batch->length);
    h2o_nif_batch_req_t *req = batch->req;
    if (req == NULL) {
        req = ((void *)batch) + sizeof(h2o_nif_batch_t);
    } else {
        req = ((void *)batch) + sizeof(h2o_nif_batch_t) + (sizeof(h2o_nif_batch_req_t) * (batch->num_req - 1));
        req->next = batch->req;
    }
    batch->req = req;
    atomic_init(&req->refc, 0);
    return req;
}
