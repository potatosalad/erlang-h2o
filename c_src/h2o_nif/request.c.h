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

static void
_h2o_nif_request_reply_4(struct _h2o_nif_request_reply_4_s *ctx)
{
    h2o_nif_request_t *request = ctx->request;
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
