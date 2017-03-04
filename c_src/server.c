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

static void *h2o_nif_server_run_loop(void *arg);
static void notify_all_threads(h2o_nif_server_t *server);
static int num_connections(h2o_nif_server_t *server, int delta);
static unsigned long num_sessions(h2o_nif_server_t *server, int delta);
static void on_accept(h2o_socket_t *listener, const char *err);
static void on_server_notification(h2o_multithread_receiver_t *receiver, h2o_linklist_t *messages);
static void on_socketclose(void *data);
static void set_cloexec(int fd);
static void update_listener_state(h2o_nif_srv_listen_t *listeners);

// h2o_nif_server_t *
// h2o_nif_server_alloc(ErlNifEnv *env)
// {
//     h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
//     ErlNifResourceType *server_type = priv_data->server;
//     h2o_nif_server_t *server = enif_alloc_resource(server_type, sizeof(*server));
//     (void)memset(server, 0, sizeof(*server));
//     return server;
// }

// void
// h2o_nif_server_release(h2o_nif_server_t *server)
// {
//     if (server == NULL)
//         return;
//     (void) enif_release_resource((void *)server);
// }

h2o_nif_server_t *
h2o_nif_server_create(ErlNifEnv *env)
{
    h2o_nif_server_t *server = enif_alloc(sizeof(*server));
    if (server == NULL) {
        return NULL;
    }
    (void)memset(server, 0, sizeof(*server));
    if (!h2o_nif_config_init(&server->config)) {
        (void)enif_free(server);
        return NULL;
    }
    server->launch_time = time(NULL);
    server->threads = NULL;
    server->shutdown_requested = 0;
    server->initialized_threads = 0;
    server->shutdown_threads = 0;
    server->state._num_connections = 0;
    server->state._num_sessions = 0;
    h2o_nif_port_t *port = h2o_nif_port_create(env, NULL);
    if (port == NULL) {
        (void)h2o_nif_config_dispose(&server->config);
        (void)enif_free(server);
        return NULL;
    }
    port->type = H2O_NIF_PORT_TYPE_SERVER;
    port->on_close = h2o_nif_server_on_close;
    port->data = (void *)server;
    server->port = port;
    return server;
}

int
h2o_nif_server_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_server_t **serverp)
{
    h2o_nif_port_t *port = NULL;

    if (serverp != NULL) {
        *serverp = NULL;
    }

    if (!h2o_nif_port_get(env, id, &port) || port->type != H2O_NIF_PORT_TYPE_SERVER || port->data == NULL) {
        return 0;
    }

    if (serverp != NULL) {
        *serverp = (h2o_nif_server_t *)port->data;
    }

    return 1;
}

// int
// h2o_nif_server_init(h2o_nif_server_t *server)
// {
//     if (!h2o_nif_config_init(&server->config)) {
//         return 0;
//     }
//     server->status = H2O_NIF_SRV_S_OPEN;
//     server->launch_time = time(NULL);
//     server->threads = NULL;
//     server->shutdown_requested = 0;
//     server->initialized_threads = 0;
//     server->state._num_connections = 0;
//     server->state._num_sessions = 0;
//     return 1;
// }

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
h2o_nif_server_on_close(ErlNifEnv *env, h2o_nif_port_t *port)
{
    if (port->data != NULL) {
        h2o_nif_server_t *server = (h2o_nif_server_t *)port->data;
        if (H2O_NIF_PORT_IS_STARTED(port)) {
            // TRACE_F("attempting to shut down server\n");
            server->shutdown_requested = 1;
            (void)notify_all_threads(server);
            while (server->initialized_threads != server->config.num_threads) {
                // TRACE_F("startup waiting on %d of %d threads\n", server->initialized_threads, server->config.num_threads);
                __asm__ __volatile__("pause" ::: "memory");
            }
            (void)notify_all_threads(server);
            while (server->initialized_threads != server->shutdown_threads) {
                // TRACE_F("shutdown waiting on %d of %d threads\n", server->initialized_threads, server->shutdown_threads);
                __asm__ __volatile__("pause" ::: "memory");
                // (void)notify_all_threads(server);
            }
            {
                size_t i;
                void *exit_value = NULL;
                h2o_nif_srv_thread_t *thread = NULL;
                for (i = 0; i != server->config.num_threads; ++i) {
                    // TRACE_F("joining thread %d\n", i);
                    thread = &server->threads[i];
                    (void)enif_thread_join(thread->tid, &exit_value);
                    (void)memset(thread, 0, sizeof(*thread));
                }
                (void)enif_free(server->threads);
                server->threads = NULL;
            }
        }
        (void)h2o_nif_config_dispose(&server->config);
        (void)memset(server, 0, sizeof(*server));
        (void)enif_free(port->data);
        port->data = NULL;
    }
    // server->port = NULL;
    // (void)enif_free(server);
    return;
}

// void
// h2o_nif_server_dtor(ErlNifEnv *env, void *obj)
// {
//     return;
// }

int
h2o_nif_server_start(ErlNifEnv *env, h2o_nif_server_t *server)
{
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
    server->port->state |= H2O_NIF_PORT_STATE_STARTED;

    /* start the threads */
    server->threads = enif_alloc(sizeof(server->threads[0]) * config->num_threads);
    (void)memset(server->threads, 0, sizeof(server->threads[0]) * config->num_threads);
    size_t i;
    for (i = 0; i != config->num_threads; ++i) {
        h2o_nif_srv_thread_t *thread = &server->threads[i];
        thread->server = server;
        thread->idx = i;
        (void)enif_thread_create("h2o_nif_srv_thread", &thread->tid, h2o_nif_server_run_loop, (void *)thread, NULL);
    }

    return 1;
}

h2o_iovec_t
h2o_nif_server_on_extra_status(void *unused, h2o_globalconf_t *_conf, h2o_req_t *req)
{
#define BUFSIZE (16 * 1024)
    h2o_nif_config_t *config = (h2o_nif_config_t *)_conf;
    h2o_nif_server_t *server = (h2o_nif_server_t *)_conf;
    h2o_iovec_t ret;
    char current_time[H2O_TIMESTR_LOG_LEN + 1], restart_time[H2O_TIMESTR_LOG_LEN + 1];
    const char *generation;
    time_t now = time(NULL);

    h2o_time2str_log(current_time, now);
    h2o_time2str_log(restart_time, server->launch_time);
    if ((generation = getenv("SERVER_STARTER_GENERATION")) == NULL)
        generation = "null";

    ret.base = h2o_mem_alloc_pool(&req->pool, BUFSIZE);
    ret.len = snprintf(ret.base, BUFSIZE, ",\n"
                                          " \"server-version\": \"" H2O_VERSION "\",\n"
                                          " \"openssl-version\": \"%s\",\n"
                                          " \"current-time\": \"%s\",\n"
                                          " \"restart-time\": \"%s\",\n"
                                          " \"uptime\": %" PRIu64 ",\n"
                                          " \"generation\": %s,\n"
                                          " \"connections\": %d,\n"
                                          " \"max-connections\": %d,\n"
                                          " \"listeners\": %zu,\n"
                                          " \"worker-threads\": %zu,\n"
                                          " \"num-sessions\": %lu",
                       SSLeay_version(SSLEAY_VERSION), current_time, restart_time, (uint64_t)(now - server->launch_time),
                       generation, num_connections(server, 0), config->max_connections, config->num_listeners, config->num_threads,
                       num_sessions(server, 0));
    assert(ret.len < BUFSIZE);

    return ret;
#undef BUFSIZE
}

// static void
// h2o_nif_context_dispose(h2o_context_t *ctx)
// {
//     h2o_globalconf_t *config = ctx->globalconf;
//     size_t i, j;

//     for (i = 0; config->hosts[i] != NULL; ++i) {
//         h2o_hostconf_t *hostconf = config->hosts[i];
//         for (j = 0; j != hostconf->paths.size; ++j) {
//             h2o_pathconf_t *pathconf = hostconf->paths.entries + j;
//             h2o_context_dispose_pathconf_context(ctx, pathconf);
//         }
//         h2o_context_dispose_pathconf_context(ctx, &hostconf->fallback_path);
//     }

//     TRACE_F("got to here\n");

//     free(ctx->_pathconfs_inited.entries);
//     free(ctx->_module_configs);
//     TRACE_F("dispose zero_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->zero_timeout);
//     TRACE_F("dispose one_sec_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->one_sec_timeout);
//     TRACE_F("dispose hundred_ms_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->hundred_ms_timeout);
//     TRACE_F("dispose handshake_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->handshake_timeout);
//     TRACE_F("dispose http1.req_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->http1.req_timeout);
//     TRACE_F("dispose http2.idle_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->http2.idle_timeout);
//     TRACE_F("dispose http2.graceful_shutdown_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->http2.graceful_shutdown_timeout);
//     TRACE_F("dispose proxy.io_timeout\n");
//     h2o_timeout_dispose(ctx->loop, &ctx->proxy.io_timeout);
//     /* what should we do here? assert(!h2o_linklist_is_empty(&ctx->http2._conns); */

//     h2o_filecache_destroy(ctx->filecache);
//     ctx->filecache = NULL;

//     /* clear storage */
//     for (i = 0; i != ctx->storage.size; ++i) {
//         h2o_context_storage_item_t *item = ctx->storage.entries + i;
//         if (item->dispose != NULL) {
//             item->dispose(item->data);
//         }
//     }
//     free(ctx->storage.entries);

//     /* TODO assert that the all the getaddrinfo threads are idle */
//     h2o_multithread_unregister_receiver(ctx->queue, &ctx->receivers.hostinfo_getaddr);
//     h2o_multithread_destroy_queue(ctx->queue);
// }

static void
h2o_nif_context_clear_timeout(h2o_loop_t *loop, h2o_timeout_t *timeout)
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
h2o_nif_context_clear_timeouts(h2o_context_t *ctx)
{
    // TRACE_F("clear zero_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->zero_timeout);
    // TRACE_F("clear one_sec_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->one_sec_timeout);
    // TRACE_F("clear hundred_ms_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->hundred_ms_timeout);
    // TRACE_F("clear handshake_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->handshake_timeout);
    // TRACE_F("clear http1.req_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->http1.req_timeout);
    // TRACE_F("clear http2.idle_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->http2.idle_timeout);
    // TRACE_F("clear http2.graceful_shutdown_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->http2.graceful_shutdown_timeout);
    // TRACE_F("clear proxy.io_timeout\n");
    (void)h2o_nif_context_clear_timeout(ctx->loop, &ctx->proxy.io_timeout);
    return;
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

    (void)h2o_context_init(&thread->ctx.super, h2o_evloop_create(), &config->globalconf);
    (void)h2o_multithread_register_receiver(thread->ctx.super.queue, &thread->server_notifications, on_server_notification);
    (void)h2o_multithread_register_receiver(thread->ctx.super.queue, &thread->memcached, h2o_memcached_receiver);

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

    __sync_fetch_and_add(&server->initialized_threads, 1);
    /* the main loop */
    while (1) {
        if (server->shutdown_requested)
            break;
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
    // (void)enif_mutex_lock(h2o_nif_mutex);
    (void)h2o_multithread_unregister_receiver(thread->ctx.super.queue, &thread->memcached);
    (void)h2o_multithread_unregister_receiver(thread->ctx.super.queue, &thread->server_notifications);
    (void)h2o_nif_context_clear_timeouts(&thread->ctx.super);
    (void)h2o_context_dispose(&thread->ctx.super);
    // (void)enif_mutex_unlock(h2o_nif_mutex);

    /* destroy the loop */
    (void)h2o_evloop_destroy(thread->ctx.super.loop);

    /* free the listeners */
    (void)enif_free(listeners);

    __sync_fetch_and_add(&server->shutdown_threads, 1);

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
