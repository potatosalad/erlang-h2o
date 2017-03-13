// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "handler.h"
#include "server.h"
#include "request.h"

// #define H2O_NIF_CONFIG(p)	(h2o_nif_config_t *)((p)->global)
// #define H2O_DRV_SERVER(c)	(h2o_drv_server_t *)((void *)(c) - sizeof (h2o_drv_port_t))

static H2O_NIF_RETURN h2o_nif_handler_on_close(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx);
static void h2o_nif_handler_dtor(void *block);

static int on_accept_http(h2o_nif_port_t *parent, h2o_nif_port_listen_t *listen, h2o_nif_port_t **portp);

// static void on_context_init(h2o_handler_t *_handler, h2o_context_t *ctx);
/* Request Handlers */
static int on_req_http(h2o_handler_t *super, h2o_req_t *req);
// static int on_req_websocket(h2o_handler_t *super, h2o_req_t *req);

// h2o_nif_handler_t *
// h2o_nif_handler_alloc(ErlNifEnv *env)
// {
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *handler_type = priv_data->handler;
//     h2o_nif_handler_t *handler = enif_alloc_resource(handler_type, sizeof(*handler));
//     if (handler == NULL) {
//         return NULL;
//     }
//     (void)memset(handler, 0, sizeof(*handler));
//     handler->priv_data = priv_data;
//     return handler;
// }

h2o_nif_handler_ctx_t *
h2o_nif_handler_register(ErlNifEnv *env, h2o_pathconf_t *pathconf, ERL_NIF_TERM tag, H2O_NIF_HANDLER type)
{
    TRACE_F("h2o_nif_handler_register:%s:%d\n", __FILE__, __LINE__);
    if (pathconf == NULL) {
        return NULL;
    }
    h2o_nif_config_t *config = (h2o_nif_config_t *)pathconf->global;
    h2o_nif_server_t *server = H2O_STRUCT_FROM_MEMBER(h2o_nif_server_t, config, config);
    h2o_nif_port_t *parent = server->port;
    h2o_nif_port_t *child = NULL;

    /* allocate handler */
    h2o_nif_handler_t *handler = h2o_nif_resource_alloc(sizeof(*handler));
    if (handler == NULL) {
        return NULL;
    }
    handler->resource.dtor = h2o_nif_handler_dtor;
    handler->type = type;
    (void)h2o_nif_server_keep(server);
    handler->server = server;

    /* allocate handler context */
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)h2o_create_handler(pathconf, sizeof(*ctx));
    if (ctx == NULL) {
        (void)h2o_nif_handler_release(handler);
        return NULL;
    }
    ctx->handler = handler;
    handler->ctx = ctx;

    /* open child port */
    if (!h2o_nif_port_open(parent, &child)) {
        (void)h2o_nif_handler_release(handler);
        return NULL;
    }
    (void)h2o_nif_port_keep(child);
    handler->port = child;
    child->on_close.callback = h2o_nif_handler_on_close;
    child->type = H2O_NIF_PORT_TYPE_HANDLER;
    child->data = (void *)handler;

    /* setup handler context */
    switch (handler->type) {
    case H2O_NIF_HANDLER_HTTP:
        ctx->super.on_req = on_req_http;
        child->on_accept = on_accept_http;
        break;
    // case H2O_NIF_HANDLER_WEBSOCKET:
    //     ctx->super.on_req = on_req_websocket;
    //     break;
    }

    {
        ErlNifPid caller;
        (void)enif_self(env, &caller);
        (void)h2o_nif_port_set_owner(child, caller);
        (void)h2o_nif_port_cas_set_state(child, H2O_NIF_PORT_STATE_LISTENING);
        ERL_NIF_TERM msg = enif_make_tuple2(env, enif_make_copy(env, tag), h2o_nif_port_make(env, child));
        if (!enif_send(env, &caller, NULL, msg)) {
            TRACE_F("unable to handler term\n");
        }
    }

    return ctx;
}

static H2O_NIF_RETURN
h2o_nif_handler_on_close(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx)
{
    TRACE_F("h2o_nif_handler_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_HANDLER);
    h2o_nif_handler_t *handler = (h2o_nif_handler_t *)port->data;
    (void)h2o_nif_handler_release(handler);
    // // Unlink handler from port (handler is now unsafe to use)
    // port->data = NULL;
    // (void)enif_free(handler);
    // (void)h2o_nif_port_release(port);
    return H2O_NIF_RETURN_DONE;
}

static void
h2o_nif_handler_dtor(void *block)
{
    TRACE_F("h2o_nif_handler_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_handler_t *handler = (h2o_nif_handler_t *)block;
    h2o_nif_port_t *port = handler->port;
    h2o_nif_server_t *server = handler->server;
    h2o_nif_handler_ctx_t *ctx = handler->ctx;
    if (server != NULL) {
        (void)h2o_nif_server_release(server);
    }
    if (ctx != NULL) {
        ctx->handler = NULL;
    }
    // Unlink handler from port (handler is now unsafe to use)
    if (port != NULL) {
        port->data = NULL;
        (void)h2o_nif_port_release(port);
    }
    return;
}

static int
on_accept_http(h2o_nif_port_t *parent, h2o_nif_port_listen_t *listen, h2o_nif_port_t **portp)
{
    TRACE_F("on_accept_http:%s:%d\n", __FILE__, __LINE__);
    assert(portp != NULL);
    h2o_nif_handler_http_t *port_listen = (h2o_nif_handler_http_t *)listen;
    h2o_req_t *req = port_listen->req;
    h2o_nif_handler_t *handler = port_listen->handler;
    h2o_nif_port_t *child = NULL;
    port_listen->req = NULL;
    port_listen->handler = NULL;
    if (!h2o_nif_request_accept_http(parent, handler, req, &child)) {
        TRACE_F("error accepting HTTP request\n");
        *portp = child;
        return 0;
    }
    ((h2o_nif_request_t *)child->data)->start = port_listen->start;
    ((h2o_nif_request_t *)child->data)->before_accept = port_listen->super.before_accept;
    (void)gettimeofday(&(((h2o_nif_request_t *)child->data)->after_accept), NULL);
    (void)h2o_nif_handler_release(handler);
    *portp = child;
    return 1;
}

static int
on_req_http(h2o_handler_t *super, h2o_req_t *req)
{
    // h2o_drv_handler_t *handler = (h2o_drv_handler_t *)_handler;
    TRACE_F("on_req_http:%s:%d\n", __FILE__, __LINE__);
    struct timeval start;
    (void)gettimeofday(&start, NULL);
    h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)super;
    h2o_nif_handler_t *handler = ctx->handler;
    h2o_nif_port_t *port = handler->port;
    h2o_nif_handler_http_t *port_listen = (h2o_nif_handler_http_t *)h2o_mem_alloc_pool(&req->pool, sizeof(h2o_nif_handler_http_t));
    (void)memset(port_listen, 0, sizeof(h2o_nif_handler_http_t));
    port_listen->req = req;
    port_listen->start = start;
    (void)h2o_nif_handler_keep(handler);
    port_listen->handler = handler;
    if (!h2o_nif_port_listen_dispatch(port, (h2o_nif_port_listen_t *)port_listen)) {
        TRACE_F("unable to dispach HTTP request\n");
        // (void)enif_free(port_listen);
    } else {
        TRACE_F("dispatched HTTP request\n");
    }
    // static h2o_generator_t generator = {NULL, NULL};
    // req->res.status = 200;
    // req->res.reason = "OK";
    // h2o_add_header(&req->pool, &req->res.headers, H2O_TOKEN_CONTENT_TYPE, H2O_STRLIT("text/plain; charset=utf-8"));
    // h2o_start_response(req, &generator);
    // h2o_send(req, &req->entity, 1, 1);
    // h2o_dispose_request(req);
    // h2o_drv_request_t *request = handler->data;
    // (void) h2o_drv_request_call(request, req);
    // (void) h2o_drv_request_create(handler, req);
    return 0;
}

// static volatile sig_atomic_t h2o_nif_handler_async_id = 0;
// #define NEW_ASYNC_ID() (__sync_fetch_and_add(&h2o_nif_handler_async_id, 1))

// ERL_NIF_TERM
// h2o_nif_handler_accept(ErlNifEnv *env, h2o_nif_handler_t *handler, ERL_NIF_TERM timeout)
// {
//     // TRACE_F("h2o_nif_handler_accept:%s:%d\n", __FILE__, __LINE__);
//     ERL_NIF_TERM out;
//     (void)enif_mutex_lock(handler->req.mutex);
//     if (!h2o_linklist_is_empty(&handler->req.pending)) {
//         h2o_nif_request_t *request = (h2o_nif_request_t *)handler->req.pending.next;
//         (void)h2o_linklist_unlink(&request->_link);
//         (void)enif_mutex_unlock(handler->req.mutex);
//         // TRACE_F("returning found request\n");
//         if (request->ws != NULL) {
//             (void)enif_self(env, &request->_wspid);
//             request->wspid = &request->_wspid;
//         }
//         out = enif_make_resource(env, (void *)request);
//         (void)enif_release_resource((void *)request);
//         return out;
//     }
//     (void)enif_mutex_unlock(handler->req.mutex);
//     // TRACE_F("enqueueing accept\n");
//     h2o_nif_accept_t *acc = enif_alloc(sizeof(*acc));
//     (void)memset(acc, 0, sizeof(*acc));
//     // TRACE_F("acc next=%p, prev=%p\n", acc->_link.next, acc->_link.prev);
//     acc->_link.next = acc->_link.prev = NULL;
//     // (void)h2o_linklist_init_anchor(&acc->_link);
//     // TRACE_F("acc next=%p, prev=%p\n", acc->_link.next, acc->_link.prev);
//     acc->id = NEW_ASYNC_ID();
//     (void)enif_self(env, &acc->caller);
//     (void)enif_mutex_lock(handler->acc.mutex);
//     (void)h2o_linklist_insert(&handler->acc.pending, &acc->_link);
//     (void)enif_mutex_unlock(handler->acc.mutex);
//     out = enif_make_tuple1(env, enif_make_int(env, acc->id));
//     return out;
// }

//     if (handler->state == H2O_NIF_HDL_S_ACCEPTING) {
//         unsigned long time_left = 0;
//         int oid = 0;

//         deq_async_w_tmo(handler, &oid, &ocaller, &oreq, &otimeout, &omonitor);
//     } else {

//                 n = sizeof(desc->inet.remote);
//                 sys_memzero((char *) &remote, n);
//                 s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &n);
//                 if (s == INVALID_SOCKET) {
//                 if (sock_errno() == ERRNO_BLOCK) {
//                     ErlDrvMonitor monitor;
//                     if (driver_monitor_process(desc->inet.port, driver_caller(desc->inet.port),
//                                    &monitor) != 0) {
//                     return ctl_xerror("noproc", rbuf, rsize);
//                     }
//                     enq_async_w_tmo(INETP(desc), tbuf, INET_REQ_ACCEPT, timeout, &monitor);
//                     desc->inet.state = INET_STATE_ACCEPTING;
//                     sock_select(INETP(desc),FD_ACCEPT,1);
//                     if (timeout != INET_INFINITY) {
//                     driver_set_timer(desc->inet.port, timeout);
//                     }
//                 } else {
//                     return ctl_error(sock_errno(), rbuf, rsize);
//                 }
//                 } else {
//                 ErlDrvTermData caller = driver_caller(desc->inet.port);
//                 tcp_descriptor* accept_desc;
//                 int err;

//                 if ((accept_desc = tcp_inet_copy(desc,s,caller,&err)) == NULL) {
//                     sock_close(s);
//                     return ctl_error(err, rbuf, rsize);
//                 }
//                 /* FIXME: may MUST lock access_port
//                  * 1 - Port is accessible via the erlang:ports()
//                  * 2 - Port is accessible via callers process_info(links)
//                  */
//                 accept_desc->inet.remote = remote;
//                 SET_NONBLOCKING(accept_desc->inet.s);
//         #ifdef __WIN32__
//                 driver_select(accept_desc->inet.port, accept_desc->inet.event,
//                           ERL_DRV_READ, 1);
//         #endif
//                 accept_desc->inet.state = INET_STATE_CONNECTED;
//                 enq_async(INETP(desc), tbuf, INET_REQ_ACCEPT);
//                 async_ok_port(INETP(desc), accept_desc->inet.dport);
//                 }
//                 return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
//     }
//         case INET_REQ_ACCEPT: {  /* do async accept */
//         char tbuf[2];
//         unsigned timeout;
//         inet_address remote;
//         unsigned int n;
//         SOCKET s;

//         DEBUGF(("tcp_inet_ctl(%ld): ACCEPT\r\n", (long)desc->inet.port));
//         /* INPUT: Timeout(4) */

//         if ((desc->inet.state != INET_STATE_LISTENING && desc->inet.state != INET_STATE_ACCEPTING &&
//              desc->inet.state != INET_STATE_MULTI_ACCEPTING) || len != 4) {
//             return ctl_error(EINVAL, rbuf, rsize);
//         }

//         timeout = get_int32(buf);

//         if (desc->inet.state == INET_STATE_ACCEPTING) {
//             unsigned long time_left = 0;
//             int oid = 0;
//             ErlDrvTermData ocaller = ERL_DRV_NIL;
//             int oreq = 0;
//             unsigned otimeout = 0;
//             ErlDrvTermData caller = driver_caller(desc->inet.port);
//             MultiTimerData *mtd = NULL,*omtd = NULL;
//             ErlDrvMonitor monitor, omonitor;

//             if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) {
//             return ctl_xerror("noproc", rbuf, rsize);
//             }
//             deq_async_w_tmo(INETP(desc),&oid,&ocaller,&oreq,&otimeout,&omonitor);
//             if (otimeout != INET_INFINITY) {
//             driver_read_timer(desc->inet.port, &time_left);
//             driver_cancel_timer(desc->inet.port);
//             if (time_left <= 0) {
//                 time_left = 1;
//             }
//             omtd = add_multi_timer(&(desc->mtd), desc->inet.port, ocaller,
//                            time_left, &tcp_inet_multi_timeout);
//             }
//             enq_old_multi_op(desc, oid, oreq, ocaller, omtd, &omonitor);
//             if (timeout != INET_INFINITY) {
//             mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller,
//                           timeout, &tcp_inet_multi_timeout);
//             }
//             enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
//             desc->inet.state = INET_STATE_MULTI_ACCEPTING;
//             return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
//         } else if (desc->inet.state == INET_STATE_MULTI_ACCEPTING) {
//             ErlDrvTermData caller = driver_caller(desc->inet.port);
//             MultiTimerData *mtd = NULL;
//             ErlDrvMonitor monitor;

//             if (driver_monitor_process(desc->inet.port, caller ,&monitor) != 0) {
//             return ctl_xerror("noproc", rbuf, rsize);
//             }
//             if (timeout != INET_INFINITY) {
//             mtd = add_multi_timer(&(desc->mtd), desc->inet.port, caller,
//                           timeout, &tcp_inet_multi_timeout);
//             }
//             enq_multi_op(desc, tbuf, INET_REQ_ACCEPT, caller, mtd, &monitor);
//             return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
//         } else {
//             n = sizeof(desc->inet.remote);
//             sys_memzero((char *) &remote, n);
//             s = sock_accept(desc->inet.s, (struct sockaddr*) &remote, &n);
//             if (s == INVALID_SOCKET) {
//             if (sock_errno() == ERRNO_BLOCK) {
//                 ErlDrvMonitor monitor;
//                 if (driver_monitor_process(desc->inet.port, driver_caller(desc->inet.port),
//                                &monitor) != 0) {
//                 return ctl_xerror("noproc", rbuf, rsize);
//                 }
//                 enq_async_w_tmo(INETP(desc), tbuf, INET_REQ_ACCEPT, timeout, &monitor);
//                 desc->inet.state = INET_STATE_ACCEPTING;
//                 sock_select(INETP(desc),FD_ACCEPT,1);
//                 if (timeout != INET_INFINITY) {
//                 driver_set_timer(desc->inet.port, timeout);
//                 }
//             } else {
//                 return ctl_error(sock_errno(), rbuf, rsize);
//             }
//             } else {
//             ErlDrvTermData caller = driver_caller(desc->inet.port);
//             tcp_descriptor* accept_desc;
//             int err;

//             if ((accept_desc = tcp_inet_copy(desc,s,caller,&err)) == NULL) {
//                 sock_close(s);
//                 return ctl_error(err, rbuf, rsize);
//             }
//             /* FIXME: may MUST lock access_port
//              * 1 - Port is accessible via the erlang:ports()
//              * 2 - Port is accessible via callers process_info(links)
//              */
//             accept_desc->inet.remote = remote;
//             SET_NONBLOCKING(accept_desc->inet.s);
//     #ifdef __WIN32__
//             driver_select(accept_desc->inet.port, accept_desc->inet.event,
//                       ERL_DRV_READ, 1);
//     #endif
//             accept_desc->inet.state = INET_STATE_CONNECTED;
//             enq_async(INETP(desc), tbuf, INET_REQ_ACCEPT);
//             async_ok_port(INETP(desc), accept_desc->inet.dport);
//             }
//             return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
//         }
//         }
// }

// static int
// enq_async_w_tmo(h2o_nif_handler_t *handler, char *buf, int req, unsigned timeout, ErlDrvMonitor *monitorp)
// {
// int id = NEW_ASYNC_ID();
// h2o_nif_async_op_t *opp;
//
// if ((opp = handler->oph) == NULL) {
// /* queue empty */
// opp = handler->oph = handler->opt = handler->op_queue;
// } else if (handler->oph == handler->opt) {
// /* queue full */
// return 0;
// }
//
// opp->id = id;
// opp->caller = handler->caller;
// opp->req = req;
// opp->tmio.value = timeout;
//
// opp++;
// if (opp >= handler->op_queue + H2O_NIF_HDL_MAX_ASYNC) {
// handler->oph = handler->op_queue;
// } else {
// handler->oph = opp;
// }
//
// if ((opp = desc->oph) == NULL) /* queue empty */
// opp = desc->oph = desc->opt = desc->op_queue;
// else if (desc->oph == desc->opt) { /* queue full */
// DEBUGF(("enq(%ld): queue full\r\n", (long)desc->port));
// return -1;
// }
//
// opp->id = id;
// opp->caller = driver_caller(desc->port);
// opp->req = req;
// opp->tmo.value = timeout;
// if (monitorp != NULL) {
// memcpy(&(opp->monitor), monitorp, sizeof(ErlDrvMonitor));
// }
//
// DEBUGF(("enq(%ld): %d %ld %d\r\n", (long)desc->port, opp->id, opp->caller, opp->req));
//
// opp++;
// if (opp >= desc->op_queue + INET_MAX_ASYNC)
// desc->oph = desc->op_queue;
// else
// desc->oph = opp;
//
// if (buf != NULL)
// put_int16(id, buf);
// return 0;
// }

// static void
// on_context_init(h2o_handler_t *_handler, h2o_context_t *ctx)
// {
//     TRACE_F("on_context_init:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_hdl_child_t *child = (h2o_nif_hdl_child_t *)_handler;
//     h2o_nif_handler_t *handler = child->parent;

//     (void)enif_mutex_lock(handler->mtx);
//     ERL_NIF_TERM ref;
//     if (handler->refs == NULL) {
//         handler->refs = enif_alloc(sizeof(handler->refs[0]));
//         handler->num_refs = 1;
//         ref = handler->refs[0] = enif_make_ref(handler->env);
//     } else {
//         handler->refs = enif_realloc(handler->refs, sizeof(handler->refs[0]) * (handler->num_refs + 1));
//         handler->num_refs++;
//         ref = handler->refs[handler->num_refs - 1] = enif_make_ref(handler->env);
//     }
//     ErlNifEnv *msg_env = enif_alloc_env();
//     ERL_NIF_TERM msg =
//         enif_make_tuple3(msg_env, enif_make_atom(msg_env, "h2o_handler"), enif_make_copy(msg_env, handler->tag),
//                          enif_make_tuple2(msg_env, enif_make_copy(msg_env, ref), enif_make_resource(msg_env, (void *)handler)));
//     if (!enif_send(NULL, &handler->pid, msg_env, msg)) {
//         TRACE_F("freeing env\n");
//         (void)enif_free_env(msg_env);
//     }
//     TRACE_F("on_context_init handler=%p\n", handler);
//     (void)enif_mutex_unlock(handler->mtx);
// }

// static void on_ws_message(h2o_websocket_conn_t *conn, const struct wslay_event_on_msg_recv_arg *arg)
// {
//     TRACE_F("on_ws_message: %p\n", arg);
//     if (arg == NULL) {
//         h2o_websocket_close(conn);
//         return;
//     }

//     if (!wslay_is_ctrl_frame(arg->opcode)) {
//         TRACE_F("opcode=%d, msg=\"%.*s\"\n", arg->opcode, arg->msg_length, arg->msg);
//         struct wslay_event_msg msgarg = {arg->opcode, arg->msg, arg->msg_length};
//         wslay_event_queue_msg(conn->ws_ctx, &msgarg);
//     }
// }

// static int
// on_req_websocket(h2o_handler_t *_handler, h2o_req_t *req)
// {
//     TRACE_F("on_req_websocket:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_handler_ctx_t *ctx = (h2o_nif_handler_ctx_t *)_handler;
//     h2o_nif_handler_t *handler = ctx->handler;
//     h2o_nif_request_t *request = h2o_nif_request_create(handler, req);
//     const char *client_key;
//     if (h2o_is_websocket_handshake(req, &client_key) != 0 || client_key == NULL) {
//         return -1;
//     }
//     request->ws = h2o_upgrade_to_websocket(req, client_key, (void *)request, h2o_nif_request_on_ws_message);
//     (void)h2o_nif_request_dispatch(request);
//     return 0;
// }
