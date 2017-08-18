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
#include "server.h"

static int cloexec_pipe(int fds[2]);
static void on_read(h2o_socket_t *sock, const char *err);
// static void on_sentinel_dispose(void *data);
static void queue_cb(h2o_nif_ipc_queue_t *queue);
static int set_cloexec(int fd);

/* IPC Functions */

h2o_nif_ipc_queue_t *
h2o_nif_ipc_create_queue(h2o_loop_t *loop)
{
    h2o_nif_ipc_queue_t *queue = mem_alloc(sizeof(*queue));
    if (queue == NULL) {
        return NULL;
    }
    (void)memset(queue, 0, sizeof(*queue));
    int fds[2];
    if (cloexec_pipe(fds) != 0) {
        perror("pipe");
        abort();
    }
    (void)fcntl(fds[1], F_SETFL, O_NONBLOCK);
    queue->async.flag = (atomic_flag)ATOMIC_FLAG_INIT;
    queue->async.write = fds[1];
    queue->async.read = h2o_evloop_socket_create(loop, fds[0], 0);
    queue->async.read->data = queue;
    (void)ck_spinlock_init(&queue->receivers.lock);
    (void)h2o_linklist_init_anchor(&queue->receivers.work);
    (void)h2o_linklist_init_anchor(&queue->receivers.busy);
    (void)h2o_linklist_init_anchor(&queue->receivers.idle);
    (void)h2o_socket_read_start(queue->async.read, on_read);
    return queue;
}

void
h2o_nif_ipc_destroy_queue(h2o_nif_ipc_queue_t *queue)
{
    assert(h2o_linklist_is_empty(&queue->receivers.work));
    assert(h2o_linklist_is_empty(&queue->receivers.busy));
    assert(h2o_linklist_is_empty(&queue->receivers.idle));
    (void)atomic_flag_test_and_set(&queue->async.flag);
    (void)h2o_socket_read_stop(queue->async.read);
    (void)h2o_socket_close(queue->async.read);
    (void)close(queue->async.write);
    (void)mem_free(queue);
    return;
}

int
h2o_nif_ipc_send(h2o_nif_ipc_receiver_t *receiver, h2o_nif_ipc_message_t *message)
{
    TRACE_F("h2o_nif_ipc_send:%s:%d\n", __FILE__, __LINE__);
    assert(!h2o_linklist_is_linked(&message->link));
    (void)ck_spinlock_lock_eb(&receiver->queue->receivers.lock);
    if (h2o_linklist_is_empty(&receiver->_messages)) {
        (void)h2o_linklist_unlink(&receiver->_link);
        (void)h2o_linklist_insert(&receiver->queue->receivers.busy, &receiver->_link);
    }
    (void)h2o_linklist_insert(&receiver->_messages, &message->link);
    (void)ck_spinlock_unlock(&receiver->queue->receivers.lock);
    if (atomic_flag_test_and_set_explicit(&receiver->queue->async.flag, memory_order_relaxed) == false) {
        while (write(receiver->queue->async.write, "", 1) == -1 && errno == EINTR) {
            (void)ck_pr_stall();
        }
    }
    return 1;
}

int
h2o_nif_ipc_send_work(h2o_nif_ipc_receiver_t *receiver, h2o_nif_ipc_job_t *job)
{
    return h2o_nif_ipc_send(receiver, &job->super);
}

void
h2o_nif_ipc_link_ostream(h2o_nif_ipc_ostream_t *ostream, h2o_req_t *req, h2o_ostream_t **slot, h2o_nif_ipc_receiver_cb_t *cb,
                         h2o_nif_ipc_receiver_dtor_t *dtor)
{
    h2o_nif_ipc_queue_t *queue = h2o_nif_thread_ipc_queue_get(req);
    // h2o_nif_ipc_sentinel_t *sentinel = (void *)h2o_mem_alloc_shared(&req->pool, sizeof(*sentinel), on_sentinel_dispose);

    // SEE: h2o_add_ostream/3
    ostream->super.next = *slot;
    ostream->super.do_send = NULL;
    ostream->super.stop = NULL;
    ostream->super.start_pull = NULL;

    *slot = &ostream->super;

    ostream->req = req;
    ostream->receiver.queue = queue;
    // ostream->receiver.sentinel = sentinel;
    ostream->receiver._link = (h2o_linklist_t){NULL, NULL};
    (void)h2o_linklist_init_anchor(&ostream->receiver._messages);
    ostream->receiver._work_link = (h2o_linklist_t){NULL, NULL};
    (void)h2o_linklist_init_anchor(&ostream->receiver._work_messages);
    ostream->receiver.cb = cb;
    ostream->receiver.dtor = dtor;
    // atomic_init(&sentinel->receiver, &ostream->receiver);

    (void)ck_spinlock_lock_eb(&queue->receivers.lock);
    (void)h2o_linklist_insert(&queue->receivers.idle, &ostream->receiver._link);
    (void)ck_spinlock_unlock(&queue->receivers.lock);
}

void
h2o_nif_ipc_unlink_ostream(h2o_nif_ipc_ostream_t *ostream)
{
    assert(h2o_linklist_is_empty(&ostream->receiver._messages));
    h2o_nif_ipc_queue_t *queue = ostream->receiver.queue;
    // h2o_nif_ipc_sentinel_t *sentinel = ostream->receiver.sentinel;
    (void)ck_spinlock_lock_eb(&queue->receivers.lock);
    (void)h2o_linklist_unlink(&ostream->receiver._link);
    (void)ck_spinlock_unlock(&queue->receivers.lock);
    // if (sentinel != NULL) {
    //     ostream->receiver.sentinel = NULL;
    //     (void)atomic_store_explicit(&sentinel->receiver, NULL, memory_order_relaxed);
    //     (void)h2o_mem_release_shared((void *)sentinel);
    // }
}

void
h2o_nif_ipc_link_request(h2o_nif_ipc_request_t *request, h2o_req_t *req, h2o_nif_ipc_receiver_cb_t *cb, h2o_nif_ipc_receiver_dtor_t *dtor)
{
    h2o_nif_ipc_queue_t *queue = h2o_nif_thread_ipc_queue_get(req);
    // h2o_nif_ipc_sentinel_t *sentinel = (void *)h2o_mem_alloc_shared(&req->pool, sizeof(*sentinel), on_sentinel_dispose);

    request->req = req;
    request->receiver.queue = queue;
    // request->receiver.sentinel = sentinel;
    request->receiver._link = (h2o_linklist_t){NULL, NULL};
    (void)h2o_linklist_init_anchor(&request->receiver._messages);
    request->receiver._work_link = (h2o_linklist_t){NULL, NULL};
    (void)h2o_linklist_init_anchor(&request->receiver._work_messages);
    request->receiver.cb = cb;
    request->receiver.dtor = dtor;
    // atomic_init(&sentinel->receiver, &request->receiver);

    (void)ck_spinlock_lock_eb(&queue->receivers.lock);
    (void)h2o_linklist_insert(&queue->receivers.idle, &request->receiver._link);
    (void)ck_spinlock_unlock(&queue->receivers.lock);
}

void
h2o_nif_ipc_unlink_request(h2o_nif_ipc_request_t *request)
{
    assert(h2o_linklist_is_empty(&request->receiver._messages));
    h2o_nif_ipc_queue_t *queue = request->receiver.queue;
    // h2o_nif_ipc_sentinel_t *sentinel = request->receiver.sentinel;
    (void)ck_spinlock_lock_eb(&queue->receivers.lock);
    (void)h2o_linklist_unlink(&request->receiver._link);
    (void)ck_spinlock_unlock(&queue->receivers.lock);
    // if (sentinel != NULL) {
    //     request->receiver.sentinel = NULL;
    //     (void)atomic_store_explicit(&sentinel->receiver, NULL, memory_order_relaxed);
    //     (void)h2o_mem_release_shared((void *)sentinel);
    // }
}

/* Internal Functions */

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
    (void)queue_cb(sock->data);
}

// static void
// on_sentinel_dispose(void *data)
// {
//     TRACE_F("on_sentinel_dispose:%s:%d\n", __FILE__, __LINE__);
//     h2o_nif_ipc_sentinel_t *sentinel = data;
//     h2o_nif_ipc_receiver_t *receiver = atomic_exchange_explicit(&sentinel->receiver, NULL, memory_order_relaxed);
//     if (receiver == NULL) {
//         return;
//     }
//     receiver->sentinel = NULL;
//     if (receiver->dtor != NULL) {
//         receiver->dtor(receiver);
//     }
// }

// static inline int
// queue_is_empty(h2o_nif_ipc_queue_t *queue)
// {
//     int retval;
//     (void)ck_spinlock_lock_eb(&queue->fifo.spinlock);
//     retval = h2o_linklist_is_empty(&queue->fifo.messages);
//     (void)ck_spinlock_unlock(&queue->fifo.spinlock);
//     return retval;
// }

static void
queue_cb(h2o_nif_ipc_queue_t *queue)
{
    h2o_nif_ipc_receiver_t *receiver = NULL;
    (void)atomic_flag_clear_explicit(&queue->async.flag, memory_order_relaxed);
    (void)ck_spinlock_lock_eb(&queue->receivers.lock);
    /* detach all busy receivers from the queue */
    assert(h2o_linklist_is_empty(&queue->receivers.work));
    while (!h2o_linklist_is_empty(&queue->receivers.busy)) {
        receiver = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_receiver_t, _link, queue->receivers.busy.next);
        /* detach all the messages from the receiver */
        assert(h2o_linklist_is_empty(&receiver->_work_messages));
        (void)h2o_linklist_insert_list(&receiver->_work_messages, &receiver->_messages);
        /* relink the receiver to the idle list */
        (void)h2o_linklist_unlink(&receiver->_link);
        (void)h2o_linklist_insert(&queue->receivers.idle, &receiver->_link);
        /* link the receiver to the work list */
        assert(!h2o_linklist_is_linked(&receiver->_work_link));
        (void)h2o_linklist_insert(&queue->receivers.work, &receiver->_work_link);
    }
    (void)ck_spinlock_unlock(&queue->receivers.lock);
    /* dispatch the messages */
    while (!h2o_linklist_is_empty(&queue->receivers.work)) {
        receiver = H2O_STRUCT_FROM_MEMBER(h2o_nif_ipc_receiver_t, _work_link, queue->receivers.work.next);
        (void)h2o_linklist_unlink(&receiver->_work_link);
        (void)receiver->cb(receiver, &receiver->_work_messages);
        assert(h2o_linklist_is_empty(&receiver->_work_messages));
    }
}

static int
set_cloexec(int fd)
{
    return fcntl(fd, F_SETFD, FD_CLOEXEC) != -1 ? 0 : -1;
}
