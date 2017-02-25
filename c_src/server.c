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
#include "yoml-parser.h"
// #include "neverbleed.h"
#include "h2o.h"
#include "h2o/configurator.h"
#include "h2o/http1.h"
#include "h2o/http2.h"
#include "h2o/serverutil.h"
// #include "standalone.h"
#include "server.h"
#include "config.h"
#include "handler.h"

static void *h2o_nif_server_loop(void *arg);
static void notify_all_threads(h2o_nif_server_t *server);
static int num_connections(h2o_nif_server_t *server, int delta);
static unsigned long num_sessions(h2o_nif_server_t *server, int delta);
static void on_accept(h2o_socket_t *listener, const char *err);
static void on_server_notification(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void on_socketclose(void *data);
static void set_cloexec(int fd);
static void update_listener_state(h2o_nif_srv_listen_t *listeners);

h2o_nif_server_t *
h2o_nif_server_alloc(ErlNifEnv *env)
{
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *server_type = priv_data->server;
    h2o_nif_server_t *server = enif_alloc_resource(server_type, sizeof(*server));
    (void)memset(server, 0, sizeof(*server));
    return server;
}

// void
// h2o_nif_server_release(h2o_nif_server_t *server)
// {
//     if (server == NULL)
//         return;
//     (void) enif_release_resource((void *)server);
// }

int
h2o_nif_server_init(h2o_nif_server_t *server)
{
    if (!h2o_nif_config_init(&server->config)) {
        return 0;
    }
    server->status = H2O_NIF_SRV_S_OPEN;
    server->launch_time = time(NULL);
    server->threads = NULL;
    server->shutdown_requested = 0;
    server->initialized_threads = 0;
    server->state._num_connections = 0;
    server->state._num_sessions = 0;
    return 1;
}

// ERL_NIF_TERM
// h2o_nif_server_is_running(ErlNifEnv *env, const ERL_NIF_TERM server_term)
// {
//     // TRACE_F("h2o_nif_server_is_running:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *server_type = priv_data->server;
//     h2o_nif_server_t *server = NULL;
//     if (!enif_get_resource(env, server_term, server_type, (void **)&server)) {
//         return enif_make_badarg(env);
//     }
//     if (server->running) {
//         return enif_make_atom(env, "true");
//     } else {
//         return enif_make_atom(env, "false");
//     }
// }

void
h2o_nif_server_dtor(ErlNifEnv *env, void *obj)
{
    return;
}

int
h2o_nif_server_start(ErlNifEnv *env, h2o_nif_server_t *server)
{
    if (server->status != H2O_NIF_SRV_S_IDLE) {
        return enif_make_badarg(env);
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
                fprintf(stderr, "[INFO] raised RLIMIT_NOFILE to %d\n", (int)limit.rlim_cur);
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

    /* switch to running status */
    server->status = H2O_NIF_SRV_S_LOOP;

    /* start the threads */
    server->threads = enif_alloc(sizeof(server->threads[0]) * config->num_threads);
    size_t i;
    for (i = 0; i != config->num_threads; ++i) {
        h2o_nif_srv_thread_t *thread = &server->threads[i];
        thread->server = server;
        thread->idx = i;
        (void)enif_thread_create("h2o_nif_srv_thread", &thread->tid, h2o_nif_server_loop, (void *)thread, NULL);
    }

    return 1;
}

static void *
h2o_nif_server_loop(void *arg)
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
    size_t i;

    (void)h2o_context_init(&thread->ctx, h2o_evloop_create(), &config->globalconf);
    (void)h2o_multithread_register_receiver(thread->ctx.queue, &thread->server_notifications, on_server_notification);
    (void)h2o_multithread_register_receiver(thread->ctx.queue, &thread->memcached, h2o_memcached_receiver);

    TRACE_F("enif_thread_self() = %p\n", enif_thread_self());

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
        listeners[i].accept_ctx.ctx = &thread->ctx;
        listeners[i].accept_ctx.hosts = listener_config->hosts;
        // if (listener_config->ssl.size != 0)
        //  listeners[i].accept_ctx.ssl_ctx = listener_config->ssl.entries[0]->ctx;
        listeners[i].accept_ctx.expect_proxy_line = listener_config->proxy_protocol;
        listeners[i].accept_ctx.libmemcached_receiver = &thread->memcached;
        listeners[i].sock = h2o_evloop_socket_create(thread->ctx.loop, fd, H2O_SOCKET_FLAG_DONT_READ);
        listeners[i].sock->data = listeners + i;
    }
    /* and start listening */
    (void)update_listener_state(listeners);

    __sync_fetch_and_add(&server->initialized_threads, 1);
    /* the main loop */
    while (1) {
        if (server->shutdown_requested)
            break;
        update_listener_state(listeners);
        /* run the loop once */
        (void)h2o_evloop_run(thread->ctx.loop, INT32_MAX);
        (void)h2o_filecache_clear(thread->ctx.filecache);
    }

    /* shutdown requested, unregister, close the listeners and notify the protocol handlers */
    for (i = 0; i != config->num_listeners; ++i)
        (void)h2o_socket_read_stop(listeners[i].sock);
    (void)h2o_evloop_run(thread->ctx.loop, 0);
    for (i = 0; i != config->num_listeners; ++i) {
        (void)h2o_socket_close(listeners[i].sock);
        listeners[i].sock = NULL;
    }
    (void)h2o_context_request_shutdown(&thread->ctx);

    /* wait until all the connection gets closed */
    while (num_connections(server, 0) != 0)
        h2o_evloop_run(thread->ctx.loop, INT32_MAX);

    return NULL;
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
    return __sync_fetch_and_add(&server->state._num_connections, delta);
}

static unsigned long
num_sessions(h2o_nif_server_t *server, int delta)
{
    return __sync_fetch_and_add(&server->state._num_sessions, delta);
}

static void
on_accept(h2o_socket_t *listener, const char *err)
{
    h2o_nif_srv_listen_t *ctx = listener->data;
    h2o_nif_server_t *server = ctx->thread->server;
    h2o_nif_config_t *config = &server->config;
    size_t num_accepts = config->max_connections / 16 / config->num_threads;
    if (num_accepts < 8)
        num_accepts = 8;

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
        notify_all_threads(server);
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
