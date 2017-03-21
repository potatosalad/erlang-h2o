// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include <arpa/inet.h>
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <inttypes.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <pwd.h>
#include <signal.h>
#include <spawn.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#ifdef __GLIBC__
#include <execinfo.h>
#endif

#include "server.h"
// #include "request.h"

#include <yoml-parser.h>
// #include "neverbleed.h"
#include <h2o.h>
#include <h2o/configurator.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <h2o/serverutil.h>
// #include "standalone.h"

/* Port Functions */

static ERL_NIF_TERM h2o_nif_server_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call);
static void h2o_nif_server_dtor(ErlNifEnv *env, h2o_nif_port_t *port);

int
h2o_nif_server_open(h2o_nif_server_t **serverp)
{
    assert(serverp != NULL);
    h2o_nif_server_t *server = NULL;
    if (!h2o_nif_port_open(NULL, sizeof(h2o_nif_server_t), (h2o_nif_port_t **)&server)) {
        *serverp = NULL;
        return 0;
    }
    server->super.dtor = h2o_nif_server_dtor;
    server->super.type = H2O_NIF_PORT_TYPE_SERVER;
    server->launch_time = time(NULL);
    server->threads = NULL;
    (void)atomic_init(&server->shutdown_requested, 0);
    (void)atomic_init(&server->initialized_threads, 0);
    (void)atomic_init(&server->shutdown_threads, 0);
    (void)atomic_init(&server->state._num_connections, 0);
    (void)atomic_init(&server->state._num_sessions, 0);
    if (!h2o_nif_config_init(&server->config)) {
        (void)h2o_nif_port_close(&server->super, NULL, NULL);
        *serverp = NULL;
        return 0;
    }
    (void)h2o_nif_port_keep(&server->super);
    server->super.on_close.callback = h2o_nif_server_on_close;
    *serverp = server;
    return 1;
}

static ERL_NIF_TERM
h2o_nif_server_on_close(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_server_on_close:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_SERVER);
    h2o_nif_server_t *server = (h2o_nif_server_t *)port;
    (void)h2o_nif_config_dispose(&server->config);
    (void)h2o_nif_port_release(&server->super);
    return ATOM_ok;
}

static void
h2o_nif_server_dtor(ErlNifEnv *env, h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_server_dtor:%s:%d\n", __FILE__, __LINE__);
    assert(port->type == H2O_NIF_PORT_TYPE_SERVER);
    return;
}

/* Server Functions */

static void *h2o_nif_server_run_loop(void *arg);
static void context_clear_timeout(h2o_loop_t *loop, h2o_timeout_t *timeout);
static void context_clear_timeouts(h2o_context_t *ctx);
static void notify_all_threads(h2o_nif_server_t *server);
static int num_connections(h2o_nif_server_t *server, int delta);
static unsigned long num_sessions(h2o_nif_server_t *server, int delta);
static void on_accept(h2o_socket_t *listener, const char *err);
// static void on_erlang(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void on_server_notification(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void on_socketclose(void *data);
static void set_cloexec(int fd);
static void update_listener_state(h2o_nif_srv_listen_t *listeners);

int
h2o_nif_server_start(h2o_nif_server_t *server)
{
    /* switch to running status */
    if (!h2o_nif_port_set_started(&server->super)) {
        return 0;
    }

    h2o_nif_config_t *config = &server->config;

    /* calculate defaults (note: open file cached is purged once every loop) */
    config->globalconf.filecache.capacity = config->globalconf.http2.max_concurrent_requests_per_connection * 2;

    { /* raise RLIMIT_NOFILE */
        struct rlimit limit;
        if (getrlimit(RLIMIT_NOFILE, &limit) == 0) {
            limit.rlim_cur = limit.rlim_max;
            if (setrlimit(RLIMIT_NOFILE, &limit) == 0
#ifdef __APPLE__
                || (limit.rlim_cur = OPEN_MAX, setrlimit(RLIMIT_NOFILE, &limit)) == 0
#endif
                ) {
                TRACE_F("[INFO] raised RLIMIT_NOFILE to %d\n", (int)limit.rlim_cur);
            }
        }
    }

    //          /* open the log file to redirect STDIN/STDERR to, before calling setuid */
    //          if (config->error_log != NULL) {
    //              if ((config->error_log_fd = h2o_access_log_open_log(config->error_log)) == -1) {
    //                  TRACE_F("unable to open error log\n");
    //                  (void) h2o_nif_port_unlock((h2o_drv_port_t *)server);
    //                  return -1;
    //              }
    //          }

    // { /* initialize SSL_CTXs for session resumption and ticket-based resumption (also starts memcached client threads for the
    // purpose) */
    //  size_t i;
    //  size_t j;
    //  H2O_VECTOR(SSL_CTX *) ssl_contexts = {NULL};
    //  for (i = 0; i != config->num_listeners; ++i) {
    //      for (j = 0; j != config->listeners[i]->ssl.size; ++j) {
    //          (void) h2o_vector_reserve(NULL, &ssl_contexts, ssl_contexts.size + 1);
    //          ssl_contexts.entries[ssl_contexts.size++] = conf.listeners[i]->ssl.entries[j]->ctx;
    //      }
    //  }
    //  (void) ssl_setup_session_resumption(ssl_contexts.entries, ssl_contexts.size);
    //  (void) free(ssl_contexts.entries);
    // }

    /* all setup should be complete by now */

    assert(config->num_threads != 0);

    /* start the threads */
    server->threads = enif_alloc(sizeof(server->threads[0]) * config->num_threads);
    (void)memset(server->threads, 0, sizeof(server->threads[0]) * config->num_threads);
    size_t i;
    char name[32];
    for (i = 0; i != config->num_threads; ++i) {
        h2o_nif_srv_thread_t *thread = &server->threads[i];
        thread->server = server;
        thread->idx = i;
        (void)snprintf(name, sizeof(name), "h2o_nif_srv_%zu", i);
        (void)enif_thread_create(name, &thread->tid, h2o_nif_server_run_loop, (void *)thread, NULL);
    }

    return 1;
}

static void *
h2o_nif_server_run_loop(void *arg)
{
    // TRACE_F("h2o_drv_server_loop:%s:%d\n", __FILE__, __LINE__);

    h2o_nif_srv_thread_t *thread = (h2o_nif_srv_thread_t *)arg;
    if (thread == NULL) {
        return NULL;
    }
    h2o_nif_server_t *server = thread->server;
    if (server == NULL) {
        return NULL;
    }
    h2o_nif_config_t *config = &server->config;
    h2o_nif_srv_listen_t *listeners = enif_alloc(sizeof(*listeners) * config->num_listeners);
    (void)memset(listeners, 0, sizeof(*listeners) * config->num_listeners);
    size_t i;

    thread->ctx.thread = thread;

    h2o_loop_t *loop = h2o_evloop_create();
    thread->ipc_queue = h2o_nif_ipc_create_queue(loop);
    assert(thread->ipc_queue != NULL);
    (void)h2o_context_init(&thread->ctx.super, loop, &config->globalconf);
    (void)h2o_multithread_register_receiver(thread->ctx.super.queue, &thread->server_notifications, on_server_notification);
    (void)h2o_multithread_register_receiver(thread->ctx.super.queue, &thread->memcached, h2o_memcached_receiver);
    // (void)h2o_multithread_register_receiver(thread->ctx.super.queue, &thread->erlang, on_erlang);

    // TRACE_F("enif_thread_self() = %p\n", enif_thread_self());

    /* setup listeners */
    for (i = 0; i != config->num_listeners; ++i) {
        h2o_nif_cfg_listen_t *listener_config = config->listeners[i];
        int fd;
        /* dup the listener fd for other threads than the main thread */
        if (thread->idx == 0) {
            fd = listener_config->fd;
        } else {
            if ((fd = dup(listener_config->fd)) == -1) {
                perror("failed to dup listening socket");
                abort();
            }
            (void)set_cloexec(fd);
        }
        (void)memset(listeners + i, 0, sizeof(listeners[i]));
        listeners[i].thread = thread;
        listeners[i].accept_ctx.ctx = &thread->ctx.super;
        listeners[i].accept_ctx.hosts = listener_config->hosts;
        // if (listener_config->ssl.size != 0)
        //  listeners[i].accept_ctx.ssl_ctx = listener_config->ssl.entries[0]->ctx;
        listeners[i].accept_ctx.expect_proxy_line = listener_config->proxy_protocol;
        listeners[i].accept_ctx.libmemcached_receiver = &thread->memcached;
        listeners[i].sock = h2o_evloop_socket_create(thread->ctx.super.loop, fd, H2O_SOCKET_FLAG_DONT_READ);
        listeners[i].sock->data = listeners + i;
    }
    /* and start listening */
    (void)update_listener_state(listeners);

    (void)atomic_fetch_add_explicit(&server->initialized_threads, 1, memory_order_relaxed);
    /* the main loop */
    while (1) {
        if (atomic_load_explicit(&server->shutdown_requested, memory_order_relaxed)) {
            break;
        }
        (void)update_listener_state(listeners);
        /* run the loop once */
        (void)h2o_evloop_run(thread->ctx.super.loop, INT32_MAX);
        (void)h2o_filecache_clear(thread->ctx.super.filecache);
    }

    /* shutdown requested, unregister, close the listeners and notify the protocol handlers */
    for (i = 0; i != config->num_listeners; ++i) {
        (void)h2o_socket_read_stop(listeners[i].sock);
    }
    (void)h2o_evloop_run(thread->ctx.super.loop, 0);
    for (i = 0; i != config->num_listeners; ++i) {
        (void)h2o_socket_close(listeners[i].sock);
        listeners[i].sock = NULL;
    }
    (void)h2o_context_request_shutdown(&thread->ctx.super);

    /* wait until all the connection gets closed */
    while (num_connections(server, 0) != 0) {
        (void)h2o_evloop_run(thread->ctx.super.loop, INT32_MAX);
    }

    /* dispose of the context */
    // (void)h2o_multithread_unregister_receiver(thread->ctx.super.queue, &thread->erlang);
    (void)h2o_multithread_unregister_receiver(thread->ctx.super.queue, &thread->memcached);
    (void)h2o_multithread_unregister_receiver(thread->ctx.super.queue, &thread->server_notifications);
    (void)context_clear_timeouts(&thread->ctx.super);
    (void)h2o_context_dispose(&thread->ctx.super);

    (void)h2o_nif_ipc_destroy_queue(thread->ipc_queue);
    thread->ipc_queue = NULL;

    /* destroy the loop */
    (void)h2o_evloop_destroy(thread->ctx.super.loop);

    /* free the listeners */
    (void)enif_free(listeners);

    (void)atomic_fetch_add_explicit(&server->shutdown_threads, 1, memory_order_relaxed);

    return NULL;
}

static void
context_clear_timeout(h2o_loop_t *loop, h2o_timeout_t *timeout)
{
    while (!h2o_linklist_is_empty(&timeout->_entries)) {
        // TRACE_F("there's a timeout entry\n");
        h2o_timeout_entry_t *entry = H2O_STRUCT_FROM_MEMBER(h2o_timeout_entry_t, _link, timeout->_entries.next);
        (void)h2o_linklist_unlink(&entry->_link);
        entry->registered_at = 0;
        entry->cb(entry);
        (void)h2o_timeout__do_post_callback(loop);
    }
    return;
}

static void
context_clear_timeouts(h2o_context_t *ctx)
{
    // TRACE_F("clear zero_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->zero_timeout);
    // TRACE_F("clear one_sec_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->one_sec_timeout);
    // TRACE_F("clear hundred_ms_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->hundred_ms_timeout);
    // TRACE_F("clear handshake_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->handshake_timeout);
    // TRACE_F("clear http1.req_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->http1.req_timeout);
    // TRACE_F("clear http2.idle_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->http2.idle_timeout);
    // TRACE_F("clear http2.graceful_shutdown_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->http2.graceful_shutdown_timeout);
    // TRACE_F("clear proxy.io_timeout\n");
    (void)context_clear_timeout(ctx->loop, &ctx->proxy.io_timeout);
    return;
}

static void
notify_all_threads(h2o_nif_server_t *server)
{
    unsigned i;
    for (i = 0; i != server->config.num_threads; ++i) {
        (void)h2o_multithread_send_message(&server->threads[i].server_notifications, NULL);
    }
}

static int
num_connections(h2o_nif_server_t *server, int delta)
{
    return atomic_fetch_add_explicit(&server->state._num_connections, delta, memory_order_relaxed);
}

static unsigned long
num_sessions(h2o_nif_server_t *server, int delta)
{
    return atomic_fetch_add_explicit(&server->state._num_sessions, delta, memory_order_relaxed);
}

static void
on_accept(h2o_socket_t *listener, const char *err)
{
    h2o_nif_srv_listen_t *ctx = listener->data;
    h2o_nif_server_t *server = ctx->thread->server;
    h2o_nif_config_t *config = &server->config;
    size_t num_accepts = config->max_connections / 16 / config->num_threads;
    if (num_accepts < 8) {
        num_accepts = 8;
    }

    if (err != NULL) {
        return;
    }

    do {
        h2o_socket_t *sock;
        if (num_connections(server, 0) >= config->max_connections) {
            /* The accepting socket is disactivated before entering the next in `run_loop`.
             * Note: it is possible that the server would accept at most `max_connections + num_threads` connections, since the
             * server does not check if the number of connections has exceeded _after_ epoll notifies of a new connection _but_
             * _before_ calling `accept`.  In other words t/40max-connections.t may fail.
             */
            break;
        }
        if ((sock = h2o_evloop_socket_accept(listener)) == NULL) {
            break;
        }
        (void)num_connections(server, 1);
        (void)num_sessions(server, 1);

        sock->on_close.cb = on_socketclose;
        sock->on_close.data = ctx;

        (void)h2o_accept(&ctx->accept_ctx, sock);

    } while (--num_accepts != 0);
}

// static void
// on_erlang(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages)
// {
//     while (!h2o_linklist_is_empty(messages)) {
//         h2o_multithread_message_t *message = H2O_STRUCT_FROM_MEMBER(h2o_multithread_message_t, link, messages->next);
//         h2o_nif_multithread_request_t *ctx = (h2o_nif_multithread_request_t *)message;
//         (void)h2o_linklist_unlink(&message->link);
//         (void)ctx->callback(ctx->request, ctx);
//         (void)h2o_nif_request_release(ctx->request);
//         (void)enif_free(ctx);
//     }
// }

static void
on_server_notification(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages)
{
    /* the notification is used only for exitting h2o_evloop_run; actual changes are done in the main loop of run_loop */

    while (!h2o_linklist_is_empty(messages)) {
        h2o_multithread_message_t *message = H2O_STRUCT_FROM_MEMBER(h2o_multithread_message_t, link, messages->next);
        (void)h2o_linklist_unlink(&message->link);
        (void)free(message);
    }
}

static void
on_socketclose(void *data)
{
    h2o_nif_srv_listen_t *ctx = data;
    h2o_nif_server_t *server = ctx->thread->server;
    h2o_nif_config_t *config = &server->config;

    int prev_num_connections = num_connections(server, -1);

    if (prev_num_connections == config->max_connections) {
        /* ready to accept new connections. wake up all the threads! */
        (void)notify_all_threads(server);
    }
}

static void
set_cloexec(int fd)
{
    if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1) {
        perror("failed to set FD_CLOEXEC");
        abort();
    }
}

static void
update_listener_state(h2o_nif_srv_listen_t *listeners)
{
    if (listeners == NULL) {
        return;
    }
    h2o_nif_server_t *server = listeners[0].thread->server;
    h2o_nif_config_t *config = &server->config;
    size_t i;

    if (num_connections(server, 0) < config->max_connections) {
        for (i = 0; i != config->num_listeners; ++i) {
            if (!h2o_socket_is_reading(listeners[i].sock)) {
                (void)h2o_socket_read_start(listeners[i].sock, on_accept);
            }
        }
    } else {
        for (i = 0; i != config->num_listeners; ++i) {
            if (h2o_socket_is_reading(listeners[i].sock)) {
                (void)h2o_socket_read_stop(listeners[i].sock);
            }
        }
    }
}
