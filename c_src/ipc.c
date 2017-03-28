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

#include "ipc.h"

static int cloexec_pipe(int fds[2]);
static void on_read(h2o_socket_t *sock, const char *err);
static void queue_cb(h2o_nif_ipc_queue_t *queue, int dtor);
static int set_cloexec(int fd);

h2o_nif_ipc_queue_t *
h2o_nif_ipc_create_queue(h2o_loop_t *loop)
{
    h2o_nif_ipc_queue_t *queue = enif_alloc(sizeof(*queue));
    if (queue == NULL) {
        return NULL;
    }
    (void)memset(queue, 0, sizeof(*queue));
    int fds[2];
    if (cloexec_pipe(fds) != 0) {
        perror("pipe");
        abort();
    }
    fcntl(fds[1], F_SETFL, O_NONBLOCK);
    queue->async.flag = (atomic_flag)ATOMIC_FLAG_INIT;
    queue->async.write = fds[1];
    queue->async.read = h2o_evloop_socket_create(loop, fds[0], 0);
    queue->async.read->data = queue;
    (void)ck_spinlock_init(&queue->fifo.spinlock);
    (void)h2o_linklist_init_anchor(&queue->fifo.messages);
    (void)h2o_socket_read_start(queue->async.read, on_read);
    return queue;
}

void
h2o_nif_ipc_destroy_queue(h2o_nif_ipc_queue_t *queue)
{
    (void)atomic_flag_test_and_set(&queue->async.flag);
    (void)h2o_socket_read_stop(queue->async.read);
    (void)h2o_socket_close(queue->async.read);
    (void)close(queue->async.write);
    (void)queue_cb(queue, 1);
    (void)enif_free(queue);
    return;
}

static int
cloexec_pipe(int fds[2])
{
#ifdef __linux__
    return pipe2(fds, O_CLOEXEC);
#else
    int ret = -1;
    (void)enif_mutex_lock(h2o_nif_mutex);

    if (pipe(fds) != 0)
        goto Exit;
    if (set_cloexec(fds[0]) != 0 || set_cloexec(fds[1]) != 0)
        goto Exit;
    ret = 0;

Exit:
    (void)enif_mutex_unlock(h2o_nif_mutex);
    return ret;
#endif
}

static void
on_read(h2o_socket_t *sock, const char *err)
{
    // TRACE_F("on_read:%s:%d\n", __FILE__, __LINE__);
    if (err != NULL) {
        fprintf(stderr, "pipe error\n");
        abort();
    }
    (void)h2o_buffer_consume(&sock->input, sock->input->size);
    (void)queue_cb(sock->data, 0);
}

static inline int
queue_is_empty(h2o_nif_ipc_queue_t *queue)
{
    int retval;
    (void)ck_spinlock_lock_eb(&queue->fifo.spinlock);
    retval = h2o_linklist_is_empty(&queue->fifo.messages);
    (void)ck_spinlock_unlock(&queue->fifo.spinlock);
    return retval;
}

static void
queue_cb(h2o_nif_ipc_queue_t *queue, int dtor)
{
    // TRACE_F("queue_cb:%s:%d\n", __FILE__, __LINE__);
    h2o_linklist_t messages;
    h2o_linklist_t *anchor = &messages;
    h2o_linklist_t *node = NULL;
    h2o_nif_ipc_message_t *message = NULL;
    do {
        if (!dtor) {
            (void)atomic_flag_clear_explicit(&queue->async.flag, memory_order_relaxed);
        }
        (void)h2o_linklist_init_anchor(&messages);
        (void)ck_spinlock_lock_eb(&queue->fifo.spinlock);
        (void)h2o_linklist_insert_list(&messages, &queue->fifo.messages);
        (void)ck_spinlock_unlock(&queue->fifo.spinlock);
        node = anchor->next;
        while (node != anchor) {
            message = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_message_t, _link, node);
            node = node->next;
            (void)message->cb(message);
            (void)h2o_nif_ipc_destroy_message(message);
        }
        (void)ck_pr_stall();
    } while (!queue_is_empty(queue));
    return;
}

static int
set_cloexec(int fd)
{
    return fcntl(fd, F_SETFD, FD_CLOEXEC) != -1 ? 0 : -1;
}
