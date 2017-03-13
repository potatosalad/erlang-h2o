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
static void queue_cb(h2o_nif_ipc_queue_t *queue);
static int set_cloexec(int fd);

_Atomic int foo = ATOMIC_VAR_INIT(0);

h2o_nif_ipc_queue_t *
h2o_nif_ipc_create_queue(h2o_loop_t *loop)
{
    /* Allocate */
    h2o_nif_ipc_queue_t *queue = enif_alloc(sizeof(*queue));
    if (queue == NULL) {
        return NULL;
    }
    (void)memset(queue, 0, sizeof(*queue));
    ck_fifo_mpmc_entry_t *stub = malloc(sizeof(*stub));
    // ck_fifo_spsc_entry_t *stub = malloc(sizeof(*stub));
    if (stub == NULL) {
        (void)enif_free(queue);
        return NULL;
    }
    (void)memset(stub, 0, sizeof(*stub));

    /* Initialize */
    int fds[2];
    if (cloexec_pipe(fds) != 0) {
        perror("pipe");
        abort();
    }
    fcntl(fds[1], F_SETFL, O_NONBLOCK);
    queue->flag = (atomic_flag)ATOMIC_FLAG_INIT;
    queue->write = fds[1];
    queue->read = h2o_evloop_socket_create(loop, fds[0], 0);
    queue->read->data = queue;
    (void)h2o_socket_read_start(queue->read, on_read);
    (void)ck_fifo_mpmc_init(&queue->fifo, stub);
    // (void)ck_fifo_spsc_init(&queue->fifo, stub);

    return queue;
}

void
h2o_nif_ipc_destroy_queue(h2o_nif_ipc_queue_t *queue)
{
    (void)atomic_flag_test_and_set(&queue->flag);
    (void)h2o_socket_read_stop(queue->read);
    (void)h2o_socket_close(queue->read);
    (void)close(queue->write);
    ck_fifo_mpmc_entry_t *garbage = NULL;
    // ck_fifo_spsc_entry_t *garbage = NULL;
    h2o_nif_ipc_message_t *message = NULL;
    int retval;
    do {
        retval = ck_fifo_mpmc_dequeue(&queue->fifo, (void *)&message, &garbage);
        // retval = ck_fifo_spsc_dequeue(&queue->fifo, (void *)&message);
        if (retval) {
            (void)message->callback(message->data);
            (void)enif_free(message);
            (void)free(garbage);
        }
    } while (retval != 0);
    (void)ck_fifo_mpmc_deinit(&queue->fifo, &garbage);
    // (void)ck_fifo_spsc_deinit(&queue->fifo, &garbage);
    (void)free(garbage);
    (void)enif_free(queue);
}

static int
cloexec_pipe(int fds[2])
{
// #ifdef __linux__
//     return pipe2(fds, O_CLOEXEC);
// #else
    int ret = -1;
    // pthread_mutex_lock(&cloexec_mutex);

    if (pipe(fds) != 0)
        goto Exit;
    if (set_cloexec(fds[0]) != 0 || set_cloexec(fds[1]) != 0)
        goto Exit;
    ret = 0;

Exit:
    // pthread_mutex_unlock(&cloexec_mutex);
    return ret;
// #endif
}

static void on_read(h2o_socket_t *sock, const char *err)
{
    if (err != NULL) {
        fprintf(stderr, "pipe error\n");
        abort();
    }

    (void)h2o_buffer_consume(&sock->input, sock->input->size);
    queue_cb(sock->data);
}

static void
queue_cb(h2o_nif_ipc_queue_t *queue)
{
    ck_fifo_mpmc_entry_t *garbage = NULL;
    // ck_fifo_spsc_entry_t *garbage = NULL;
    h2o_nif_ipc_message_t *message = NULL;
    int retval;
    int counter = 1;
    do {
        (void)atomic_flag_clear_explicit(&queue->flag, memory_order_relaxed);
        // retval = ck_fifo_mpmc_dequeue(&queue->fifo, (void *)&message, &garbage);
        retval = ck_fifo_mpmc_trydequeue(&queue->fifo, (void *)&message, &garbage);
        // retval = ck_fifo_spsc_dequeue(&queue->fifo, (void *)&message);
        if (retval) {
            (void)message->callback(message->data);
            (void)enif_free(message);
            // (void)free(garbage);
        } else {
            (void)ck_pr_stall();
            if ((--counter) != 0) {
                retval = 1;
            }
        }
    } while (retval != 0);
    // do {
    //     (void)atomic_flag_clear_explicit(&queue->flag, memory_order_relaxed);
    //     // retval = ck_fifo_mpmc_dequeue(&queue->fifo, (void *)&message, &garbage);
    //     retval = ck_fifo_mpmc_trydequeue(&queue->fifo, (void *)&message, &garbage);
    //     // retval = ck_fifo_spsc_dequeue(&queue->fifo, (void *)&message);
    //     if (retval) {
    //         (void)message->callback(message->data);
    //         (void)enif_free(message);
    //         // (void)free(garbage);
    //     }
    // } while (retval != 0);
    return;
}

static int
set_cloexec(int fd)
{
    return fcntl(fd, F_SETFD, FD_CLOEXEC) != -1 ? 0 : -1;
}
