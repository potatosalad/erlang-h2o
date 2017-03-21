// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_IPC_H
#define H2O_NIF_IPC_H

#include "globals.h"
#include <errno.h>

typedef struct h2o_nif_ipc_queue_s h2o_nif_ipc_queue_t;
typedef struct h2o_nif_ipc_message_s h2o_nif_ipc_message_t;
typedef void h2o_nif_ipc_callback_t(void *data);

struct h2o_nif_ipc_queue_s {
    atomic_flag flag;
    int write;
    h2o_socket_t *read;
    ck_fifo_mpmc_t fifo;
};

struct h2o_nif_ipc_message_s {
    h2o_nif_ipc_callback_t *callback;
    void *data;
};

extern h2o_nif_ipc_queue_t *h2o_nif_ipc_create_queue(h2o_loop_t *loop);
extern void h2o_nif_ipc_destroy_queue(h2o_nif_ipc_queue_t *queue);
static int h2o_nif_ipc_send(h2o_nif_ipc_queue_t *queue, h2o_nif_ipc_callback_t *callback, void *data);
static int h2o_nif_ipc_send_message(h2o_nif_ipc_queue_t *queue, h2o_nif_ipc_message_t *message);

inline int
h2o_nif_ipc_send(h2o_nif_ipc_queue_t *queue, h2o_nif_ipc_callback_t *callback, void *data)
{
    h2o_nif_ipc_message_t *message = enif_alloc(sizeof(*message));
    message->callback = callback;
    message->data = data;
    return h2o_nif_ipc_send_message(queue, message);
}

inline int
h2o_nif_ipc_send_message(h2o_nif_ipc_queue_t *queue, h2o_nif_ipc_message_t *message)
{
    ck_fifo_mpmc_entry_t *fifo_entry = (ck_fifo_mpmc_entry_t *)malloc(sizeof(*fifo_entry));
    if (fifo_entry == NULL) {
        return 0;
    }
    while (ck_fifo_mpmc_tryenqueue(&queue->fifo, fifo_entry, (void *)message) == false) {
        (void)ck_pr_stall();
    }
    if (atomic_flag_test_and_set_explicit(&queue->flag, memory_order_relaxed) == false) {
        while (write(queue->write, "", 1) == -1 && errno == EINTR) {
            (void)ck_pr_stall();
        }
    }
    return 1;
}

#endif
