// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

// static void
// _h2o_nif_request_delegate_1(h2o_nif_request_t *request, h2o_nif_multithread_request_t *ctx)
// {
//     (void)ctx;
//     (void)h2o_delegate_request_deferred(request->req, &(request->handler->ctx->super));
// }

// static ERL_NIF_TERM
// h2o_nif_request_delegate_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     h2o_nif_port_t *port = NULL;
//     h2o_nif_request_t *request = NULL;
//     if (argc != 1 || !h2o_nif_request_get(env, argv[0], &port, &request)) {
//         return enif_make_badarg(env);
//     }
//     if (!h2o_nif_request_finish(request)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }
//     h2o_nif_multithread_request_t *ctx = enif_alloc(sizeof(*ctx));
//     ctx->super.link.next = ctx->super.link.prev = NULL;
//     ctx->callback = _h2o_nif_request_delegate_1;
//     (void)h2o_nif_request_keep(request);
//     ctx->request = request;
//     h2o_nif_srv_thread_ctx_t *thread_ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
//     (void)h2o_multithread_send_message(&thread_ctx->thread->erlang, &ctx->super);
//     // (void)h2o_delegate_request_deferred(request->req, &(request->handler->ctx->super));
//     // (void)h2o_nif_request_wakeup(request);
//     (void)h2o_nif_port_release(port);
//     return ATOM_ok;
// }

// static void
// _h2o_nif_request_send_inline_2(h2o_nif_request_t *request, void *data)
// {
//     TRACE_F("_h2o_nif_request_send_inline_2\n");
//     h2o_nif_multithread_env_t *mtenv = (h2o_nif_multithread_env_t *)data;
//     ErlNifBinary body_bin;
//     assert(enif_inspect_iolist_as_binary(mtenv->env, mtenv->argv[0], &body_bin));
//     (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
//     (void)enif_clear_env(mtenv->env);
//     (void)enif_free_env(mtenv->env);
//     (void)enif_free(mtenv->argv);
//     (void)enif_free(mtenv);
// }

// static ERL_NIF_TERM
// h2o_nif_request_send_inline_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     h2o_nif_port_t *port = NULL;
//     h2o_nif_request_t *request = NULL;
//     if (argc != 2 || !h2o_nif_request_get(env, argv[0], &port, &request)) {
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary body_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[1], &body_bin)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_badarg(env);
//     }
//     if (!h2o_nif_request_finish(request)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }

//     h2o_nif_multithread_request_t *mtreq = enif_alloc(sizeof(h2o_nif_multithread_request_t));
//     assert(mtreq != NULL);
//     (void)memset(mtreq, 0, sizeof(h2o_nif_multithread_request_t));
//     h2o_nif_multithread_env_t *mtenv = enif_alloc(sizeof(h2o_nif_multithread_env_t));
//     assert(mtenv != NULL);
//     (void)memset(mtenv, 0, sizeof(h2o_nif_multithread_env_t));
//     ERL_NIF_TERM *newargv = enif_alloc(sizeof(ERL_NIF_TERM) * 1);
//     assert(newargv != NULL);
//     (void)memset(newargv, 0, sizeof(ERL_NIF_TERM) * 1);
//     mtenv->env = enif_alloc_env();
//     assert(mtenv->env != NULL);
//     mtenv->argc = 1;
//     mtenv->argv = newargv;
//     newargv[0] = enif_make_copy(mtenv->env, argv[1]);
//     mtreq->callback = _h2o_nif_request_send_inline_2;
//     (void)h2o_nif_request_keep(request);
//     mtreq->request = request;
//     mtreq->data = (void *)mtenv;
//     h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
//     (void)h2o_multithread_send_message(&ctx->thread->erlang, &mtreq->super);

//     // (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
//     // (void)h2o_nif_request_wakeup(request);
//     (void)h2o_nif_port_release(port);
//     return ATOM_ok;
// }

// static void
// _h2o_nif_request_set_status_2(h2o_nif_request_t *request, void *data)
// {
//     TRACE_F("_h2o_nif_request_set_status_2\n");
//     h2o_nif_multithread_env_t *mtenv = (h2o_nif_multithread_env_t *)data;
//     unsigned int status = 0;
//     assert(enif_get_uint(mtenv->env, mtenv->argv[0], &status));
//     request->req->res.status = status;
//     (void)enif_clear_env(mtenv->env);
//     (void)enif_free_env(mtenv->env);
//     (void)enif_free(mtenv->argv);
//     (void)enif_free(mtenv);
// }

// static ERL_NIF_TERM
// h2o_nif_request_set_status_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     h2o_nif_port_t *port = NULL;
//     h2o_nif_request_t *request = NULL;
//     if (argc != 2 || !h2o_nif_request_get(env, argv[0], &port, &request)) {
//         return enif_make_badarg(env);
//     }
//     unsigned int status = 0;
//     if (!enif_get_uint(env, argv[1], &status) || status < 100 || status > 599) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_badarg(env);
//     }
//     if (!h2o_nif_request_begin(request)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }

//     h2o_nif_multithread_request_t *mtreq = enif_alloc(sizeof(h2o_nif_multithread_request_t));
//     assert(mtreq != NULL);
//     (void)memset(mtreq, 0, sizeof(h2o_nif_multithread_request_t));
//     h2o_nif_multithread_env_t *mtenv = enif_alloc(sizeof(h2o_nif_multithread_env_t));
//     assert(mtenv != NULL);
//     (void)memset(mtenv, 0, sizeof(h2o_nif_multithread_env_t));
//     ERL_NIF_TERM *newargv = enif_alloc(sizeof(ERL_NIF_TERM) * 1);
//     assert(newargv != NULL);
//     (void)memset(newargv, 0, sizeof(ERL_NIF_TERM) * 1);
//     mtenv->env = enif_alloc_env();
//     assert(mtenv->env != NULL);
//     mtenv->argc = 1;
//     mtenv->argv = newargv;
//     newargv[0] = enif_make_copy(mtenv->env, argv[1]);
//     mtreq->callback = _h2o_nif_request_set_status_2;
//     (void)h2o_nif_request_keep(request);
//     mtreq->request = request;
//     mtreq->data = (void *)mtenv;
//     h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
//     (void)h2o_multithread_send_message(&ctx->thread->erlang, &mtreq->super);


//     // request->req->res.status = status;
//     (void)h2o_nif_request_end(request);
//     (void)h2o_nif_port_release(port);
//     return ATOM_ok;
// }

// static void
// _h2o_nif_request_add_header_3(h2o_nif_request_t *request, void *data)
// {
//     TRACE_F("_h2o_nif_request_add_header_3\n");
//     h2o_nif_multithread_env_t *mtenv = (h2o_nif_multithread_env_t *)data;
//     ErlNifBinary name_bin;
//     ErlNifBinary value_bin;
//     assert(enif_inspect_iolist_as_binary(mtenv->env, mtenv->argv[0], &name_bin));
//     assert(enif_inspect_iolist_as_binary(mtenv->env, mtenv->argv[1], &value_bin));
//     (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL, (const char *)value_bin.data, value_bin.size);
//     (void)enif_clear_env(mtenv->env);
//     (void)enif_free_env(mtenv->env);
//     (void)enif_free(mtenv->argv);
//     (void)enif_free(mtenv);
// }

// static ERL_NIF_TERM
// h2o_nif_request_add_header_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     h2o_nif_port_t *port = NULL;
//     h2o_nif_request_t *request = NULL;
//     if (argc != 3 || !h2o_nif_request_get(env, argv[0], &port, &request)) {
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary name_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[1], &name_bin)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_badarg(env);
//     }
//     ErlNifBinary value_bin;
//     if (!enif_inspect_iolist_as_binary(env, argv[2], &value_bin)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_badarg(env);
//     }
//     if (!h2o_nif_request_begin(request)) {
//         (void)h2o_nif_port_release(port);
//         return enif_make_tuple2(env, ATOM_error, ATOM_closed);
//     }
//     // h2o_nif_multithread_request_t *mtreq = h2o_mem_alloc_pool(&request->req->pool, sizeof(h2o_nif_multithread_request_t));
//     h2o_nif_multithread_request_t *mtreq = enif_alloc(sizeof(h2o_nif_multithread_request_t));
//     assert(mtreq != NULL);
//     (void)memset(mtreq, 0, sizeof(h2o_nif_multithread_request_t));
//     h2o_nif_multithread_env_t *mtenv = enif_alloc(sizeof(h2o_nif_multithread_env_t));
//     assert(mtenv != NULL);
//     (void)memset(mtenv, 0, sizeof(h2o_nif_multithread_env_t));
//     ERL_NIF_TERM *newargv = enif_alloc(sizeof(ERL_NIF_TERM) * 2);
//     assert(newargv != NULL);
//     (void)memset(newargv, 0, sizeof(ERL_NIF_TERM) * 2);
//     mtenv->env = enif_alloc_env();
//     assert(mtenv->env != NULL);
//     mtenv->argc = 2;
//     mtenv->argv = newargv;
//     newargv[0] = enif_make_copy(mtenv->env, argv[1]);
//     newargv[1] = enif_make_copy(mtenv->env, argv[2]);
//     mtreq->callback = _h2o_nif_request_add_header_3;
//     (void)h2o_nif_request_keep(request);
//     mtreq->request = request;
//     mtreq->data = (void *)mtenv;
//     h2o_nif_srv_thread_ctx_t *ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
//     (void)h2o_multithread_send_message(&ctx->thread->erlang, &mtreq->super);
//     // (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1,
//     //                             NULL, (const char *)value_bin.data, value_bin.size);
//     // (void)h2o_nif_request_wakeup(request);
//     (void)h2o_nif_request_end(request);
//     (void)h2o_nif_port_release(port);
//     return ATOM_ok;
// }

/* Response API */

struct _h2o_nif_request_reply_4_s {
    h2o_nif_request_t *request;
    unsigned int status;
    ErlNifEnv *env;
    ERL_NIF_TERM headers;
    ERL_NIF_TERM body;
};

struct r_timerdata {
    _Atomic struct timeval min;
    _Atomic struct timeval max;
    _Atomic struct timeval avg;
    // _Atomic unsigned long long min;
    // _Atomic unsigned long long max;
    // _Atomic unsigned long long avg;
};

#define R_TIMERDATA_INIT {ATOMIC_VAR_INIT(((struct timeval){2147483647,2147483647})), ATOMIC_VAR_INIT(((struct timeval){0,0})), ATOMIC_VAR_INIT(((struct timeval){0,0}))}

static struct r_timerdata r_start_to_before_accept = R_TIMERDATA_INIT;
static struct r_timerdata r_start_to_after_accept = R_TIMERDATA_INIT;
static struct r_timerdata r_start_to_before_reply = R_TIMERDATA_INIT;
static struct r_timerdata r_start_to_start_reply = R_TIMERDATA_INIT;
static struct r_timerdata r_start_to_stop_reply = R_TIMERDATA_INIT;

// static inline int
// timespec_gt(const struct timespec *lhs, const struct timespec *rhs)
// {
//     if (lhs->tv_sec == rhs->tv_sec) {
//         return (lhs->tv_nsec > rhs->tv_nsec);
//     } else {
//         return (lhs->tv_sec > rhs->tv_sec);
//     }
// }

// static inline int
// timespec_lt(const struct timespec *lhs, const struct timespec *rhs)
// {
//     if (lhs->tv_sec == rhs->tv_sec) {
//         return (lhs->tv_nsec < rhs->tv_nsec);
//     } else {
//         return (lhs->tv_sec < rhs->tv_sec);
//     }
// }

static inline int
timeval_gt(const struct timeval *lhs, const struct timeval *rhs)
{
    if (lhs->tv_sec == rhs->tv_sec) {
        return (lhs->tv_usec > rhs->tv_usec);
    } else {
        return (lhs->tv_sec > rhs->tv_sec);
    }
}

static inline int
timeval_lt(const struct timeval *lhs, const struct timeval *rhs)
{
    if (lhs->tv_sec == rhs->tv_sec) {
        return (lhs->tv_usec < rhs->tv_usec);
    } else {
        return (lhs->tv_sec < rhs->tv_sec);
    }
}

// static inline void
// timespec_average(const struct timespec *a, const struct timespec *b, struct timespec *c)
// {
//     uint64_t at;
//     uint64_t bt;
//     uint64_t ct;

//     if (a->tv_sec == 0 && a->tv_nsec == 0) {
//         c->tv_sec  = b->tv_sec;
//         c->tv_nsec = b->tv_nsec;
//         return;
//     }

//     at  = a->tv_sec;
//     at *= 1000000000;
//     at += a->tv_nsec;

//     bt  = b->tv_sec;
//     bt *= 1000000000;
//     bt += b->tv_nsec;

//     ct  = at + bt;
//     ct /= 2;

//     c->tv_sec  = ct / 1000000000;
//     c->tv_nsec = ct % 1000000000;
// }

static inline void
timeval_average(const struct timeval *a, const struct timeval *b, struct timeval *c)
{
    uint64_t at;
    uint64_t bt;
    uint64_t ct;

    if (a->tv_sec == 0 && a->tv_usec == 0) {
        c->tv_sec  = b->tv_sec;
        c->tv_usec = b->tv_usec;
        return;
    }

    at  = a->tv_sec;
    at *= 1000000;
    at += a->tv_usec;

    bt  = b->tv_sec;
    bt *= 1000000;
    bt += b->tv_usec;

    ct  = at + bt;
    ct /= 2;

    c->tv_sec  = ct / 1000000;
    c->tv_usec = ct % 1000000;
}

static inline void
// record_timerdata(struct r_timerdata *t, struct timeval *timedelta)
record_timerdata(struct r_timerdata *t, struct timeval *d)
{
    // unsigned long long d = timedelta->tv_sec * 1000000 + timedelta->tv_usec;
    struct timeval expected;
    struct timeval desired;
    int retval;
    /* min */
    do {
        expected = atomic_load_explicit(&t->min, memory_order_relaxed);
        if (timeval_lt(d, &expected)) {
            desired = *d;
            retval = atomic_compare_exchange_weak_explicit(&t->min, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        } else {
            retval = 1;
        }
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    /* max */
    do {
        expected = atomic_load_explicit(&t->max, memory_order_relaxed);
        if (timeval_gt(d, &expected)) {
            desired = *d;
            retval = atomic_compare_exchange_weak_explicit(&t->max, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        } else {
            retval = 1;
        }
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    /* avg */
    do {
        expected = atomic_load_explicit(&t->avg, memory_order_relaxed);
        (void)timeval_average(&expected, d, &desired);
        retval = atomic_compare_exchange_weak_explicit(&t->avg, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    return;
}

static void
record_request_times(h2o_nif_request_t *request)
{
    struct timeval timedelta;
    timersub(&request->before_accept, &request->start, &timedelta);
    record_timerdata(&r_start_to_before_accept, &timedelta);
    timersub(&request->after_accept, &request->before_accept, &timedelta);
    record_timerdata(&r_start_to_after_accept, &timedelta);
    timersub(&request->before_reply, &request->after_accept, &timedelta);
    record_timerdata(&r_start_to_before_reply, &timedelta);
    timersub(&request->start_reply, &request->before_reply, &timedelta);
    record_timerdata(&r_start_to_start_reply, &timedelta);
    timersub(&request->stop_reply, &request->start_reply, &timedelta);
    record_timerdata(&r_start_to_stop_reply, &timedelta);
    return;
}

static inline ERL_NIF_TERM
_build_timeval(ErlNifEnv *env, struct timeval ts)
{
    return enif_make_tuple2(env, enif_make_long(env, ts.tv_sec), enif_make_long(env, ts.tv_usec));
}

static inline ERL_NIF_TERM
_build_timerdata(ErlNifEnv *env, struct r_timerdata *td)
{
    ERL_NIF_TERM stat[3];
    unsigned j = 0;
    stat[j++] = enif_make_tuple2(env, ATOM_min, _build_timeval(env, atomic_load_explicit(&td->min, memory_order_relaxed)));
    stat[j++] = enif_make_tuple2(env, ATOM_max, _build_timeval(env, atomic_load_explicit(&td->max, memory_order_relaxed)));
    stat[j++] = enif_make_tuple2(env, ATOM_avg, _build_timeval(env, atomic_load_explicit(&td->avg, memory_order_relaxed)));
    return enif_make_list_from_array(env, stat, j);
}

static ERL_NIF_TERM
h2o_nif_request_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM list[5];
    unsigned i = 0;
    list[i++] = enif_make_tuple2(env, enif_make_atom(env, "s2ba"), _build_timerdata(env, &r_start_to_before_accept));
    list[i++] = enif_make_tuple2(env, enif_make_atom(env, "s2aa"), _build_timerdata(env, &r_start_to_after_accept));
    list[i++] = enif_make_tuple2(env, enif_make_atom(env, "s2br"), _build_timerdata(env, &r_start_to_before_reply));
    list[i++] = enif_make_tuple2(env, enif_make_atom(env, "s2sr"), _build_timerdata(env, &r_start_to_start_reply));
    list[i++] = enif_make_tuple2(env, enif_make_atom(env, "s2st"), _build_timerdata(env, &r_start_to_stop_reply));
    return enif_make_list_from_array(env, list, i);
}

static void
_h2o_nif_request_reply_4(struct _h2o_nif_request_reply_4_s *ctx)
{
    h2o_nif_request_t *request = ctx->request;
    (void)gettimeofday(&request->start_reply, NULL);
    // (void)getmonotonictime(&request->start_reply);
    ErlNifBinary body_bin;
    request->req->res.status = ctx->status;
    {
        ERL_NIF_TERM key;
        ERL_NIF_TERM value;
        ErlNifMapIterator iter;
        ErlNifBinary name_bin;
        ErlNifBinary value_bin;
        assert(enif_map_iterator_create(ctx->env, ctx->headers, &iter, ERL_NIF_MAP_ITERATOR_FIRST));
        while (enif_map_iterator_get_pair(ctx->env, &iter, &key, &value)) {
            if (enif_inspect_iolist_as_binary(ctx->env, key, &name_bin) && enif_inspect_iolist_as_binary(ctx->env, value, &value_bin)) {
                (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL, (const char *)value_bin.data, value_bin.size);
            }
            (void)enif_map_iterator_next(ctx->env, &iter);
        }
        (void)enif_map_iterator_destroy(ctx->env, &iter);
    }
    assert(enif_inspect_iolist_as_binary(ctx->env, ctx->body, &body_bin));
    (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
    (void)enif_free_env(ctx->env);
    (void)enif_free(ctx);
    (void)gettimeofday(&request->stop_reply, NULL);
    // (void)getmonotonictime(&request->stop_reply);
    (void)record_request_times(request);
    (void)h2o_nif_request_release(request);
}

// static void
// _h2o_nif_request_reply_4(h2o_nif_request_t *request, h2o_nif_multithread_request_t *super)
// {
//     // TRACE_F("_h2o_nif_request_reply_4\n");
//     h2o_nif_multithread_request_args_t *ctx = (h2o_nif_multithread_request_args_t *)super;
//     assert(ctx->argc == 3);
//     unsigned int status;
//     ErlNifBinary body_bin;
//     assert(enif_get_uint(ctx->env, ctx->argv[0], &status));
//     request->req->res.status = status;
//     {
//         ERL_NIF_TERM key;
//         ERL_NIF_TERM value;
//         ErlNifMapIterator iter;
//         ErlNifBinary name_bin;
//         ErlNifBinary value_bin;
//         assert(enif_map_iterator_create(ctx->env, ctx->argv[1], &iter, ERL_NIF_MAP_ITERATOR_FIRST));
//         while (enif_map_iterator_get_pair(ctx->env, &iter, &key, &value)) {
//             if (enif_inspect_iolist_as_binary(ctx->env, key, &name_bin) && enif_inspect_iolist_as_binary(ctx->env, value, &value_bin)) {
//                 (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL, (const char *)value_bin.data, value_bin.size);
//             }
//             (void)enif_map_iterator_next(ctx->env, &iter);
//         }
//         (void)enif_map_iterator_destroy(ctx->env, &iter);
//     }
//     assert(enif_inspect_iolist_as_binary(ctx->env, ctx->argv[2], &body_bin));
//     (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
//     (void)enif_free_env(ctx->env);
// }

static ERL_NIF_TERM
h2o_nif_request_reply_4(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    h2o_nif_request_t *request = NULL;
    if (argc != 4 || !h2o_nif_request_get(env, argv[0], &port, &request)) {
        return enif_make_badarg(env);
    }
    unsigned int status;
    if (!enif_get_uint(env, argv[1], &status) || status < 100 || status > 599 || !enif_is_map(env, argv[2]) || (!enif_is_binary(env, argv[3]) && !enif_is_list(env, argv[3]))) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    if (!h2o_nif_request_finish(request)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    (void)gettimeofday(&request->before_reply, NULL);
    // (void)getmonotonictime(&request->before_reply);
    // unsigned int status;
    // ErlNifBinary body_bin;
    // assert(enif_get_uint(env, argv[1], &status));
    // request->req->res.status = status;
    // {
    //     ERL_NIF_TERM key;
    //     ERL_NIF_TERM value;
    //     ErlNifMapIterator iter;
    //     ErlNifBinary name_bin;
    //     ErlNifBinary value_bin;
    //     assert(enif_map_iterator_create(env, argv[2], &iter, ERL_NIF_MAP_ITERATOR_FIRST));
    //     while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
    //         if (enif_inspect_iolist_as_binary(env, key, &name_bin) && enif_inspect_iolist_as_binary(env, value, &value_bin)) {
    //             (void)h2o_add_header_by_str(&request->req->pool, &request->req->res.headers, (const char *)name_bin.data, name_bin.size, 1, NULL, (const char *)value_bin.data, value_bin.size);
    //         }
    //         (void)enif_map_iterator_next(env, &iter);
    //     }
    //     (void)enif_map_iterator_destroy(env, &iter);
    // }
    // assert(enif_inspect_iolist_as_binary(env, argv[3], &body_bin));
    // (void)h2o_send_inline(request->req, (const char *)body_bin.data, body_bin.size);
    // (void)h2o_nif_request_wakeup(request);

    // ErlNifEnv *req_env = enif_alloc_env();
    // h2o_nif_multithread_request_args_t *ctx = enif_alloc(sizeof(h2o_nif_multithread_request_args_t));
    // (void)memset(ctx, 0, sizeof(h2o_nif_multithread_request_args_t));
    // ctx->super.callback = _h2o_nif_request_reply_4;
    // (void)h2o_nif_request_keep(request);
    // ctx->super.request = request;
    // ctx->env = req_env;
    // ctx->argc = 3;
    // ctx->argv[0] = enif_make_copy(req_env, argv[1]);
    // ctx->argv[1] = enif_make_copy(req_env, argv[2]);
    // ctx->argv[2] = enif_make_copy(req_env, argv[3]);
    // h2o_nif_srv_thread_ctx_t *thread_ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
    // (void)h2o_multithread_send_message(&thread_ctx->thread->erlang, &ctx->super.super);

    ErlNifEnv *req_env = enif_alloc_env();
    struct _h2o_nif_request_reply_4_s *ctx = enif_alloc(sizeof(*ctx));
    (void)h2o_nif_request_keep(request);
    ctx->request = request;
    ctx->status = status;
    ctx->env = req_env;
    ctx->headers = enif_make_copy(req_env, argv[2]);
    ctx->body = enif_make_copy(req_env, argv[3]);
    h2o_nif_srv_thread_ctx_t *thread_ctx = (h2o_nif_srv_thread_ctx_t *)request->req->conn->ctx;
    (void)h2o_nif_ipc_send(thread_ctx->thread->ipc_queue, (void (*)(void *))_h2o_nif_request_reply_4, (void *)ctx);

    (void)h2o_nif_port_release(port);
    return ATOM_ok;
}
