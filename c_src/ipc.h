// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_IPC_H
#define H2O_NIF_IPC_H

#include "globals.h"
#include <errno.h>

typedef struct h2o_nif_ipc_queue_s h2o_nif_ipc_queue_t;
typedef struct h2o_nif_ipc_message_s h2o_nif_ipc_message_t;
typedef struct h2o_nif_ipc_job_s h2o_nif_ipc_job_t;
typedef struct h2o_nif_ipc_receiver_s h2o_nif_ipc_receiver_t;
typedef struct h2o_nif_ipc_ostream_s h2o_nif_ipc_ostream_t;
typedef struct h2o_nif_ipc_request_s h2o_nif_ipc_request_t;
typedef void h2o_nif_ipc_receiver_cb_t(h2o_nif_ipc_receiver_t *receiver, h2o_linklist_t *messages);
typedef void h2o_nif_ipc_receiver_dtor_t(h2o_nif_ipc_receiver_t *receiver);
typedef void h2o_nif_ipc_job_cb_t(h2o_nif_ipc_receiver_t *receiver, h2o_nif_ipc_job_t *job);

struct h2o_nif_ipc_queue_s {
    struct {
        atomic_flag flag;
        int write;
        h2o_socket_t *read;
    } async;
    struct {
        ck_spinlock_t lock;
        h2o_linklist_t work;
        h2o_linklist_t busy;
        h2o_linklist_t idle;
    } receivers;
};

struct h2o_nif_ipc_message_s {
    h2o_linklist_t link;
};

struct h2o_nif_ipc_job_s {
    h2o_nif_ipc_message_t super;
    h2o_nif_ipc_job_cb_t *cb;
};

struct h2o_nif_ipc_receiver_s {
    h2o_nif_ipc_queue_t *queue;
    h2o_linklist_t _link;
    h2o_linklist_t _messages;
    h2o_linklist_t _work_link;
    h2o_linklist_t _work_messages;
    h2o_nif_ipc_receiver_cb_t *cb;
    h2o_nif_ipc_receiver_dtor_t *dtor;
};

struct h2o_nif_ipc_ostream_s {
    h2o_ostream_t super;
    h2o_req_t *req;
    h2o_nif_ipc_receiver_t receiver;
};

struct h2o_nif_ipc_request_s {
    h2o_req_t *req;
    h2o_nif_ipc_receiver_t receiver;
};

extern h2o_nif_ipc_queue_t *h2o_nif_ipc_create_queue(h2o_loop_t *loop);
extern void h2o_nif_ipc_destroy_queue(h2o_nif_ipc_queue_t *queue);
extern int h2o_nif_ipc_send(h2o_nif_ipc_receiver_t *receiver, h2o_nif_ipc_message_t *message);
extern int h2o_nif_ipc_send_work(h2o_nif_ipc_receiver_t *receiver, h2o_nif_ipc_job_t *job);
extern void h2o_nif_ipc_link_ostream(h2o_nif_ipc_ostream_t *ostream, h2o_req_t *req, h2o_ostream_t **slot,
                                     h2o_nif_ipc_receiver_cb_t *cb, h2o_nif_ipc_receiver_dtor_t *dtor);
extern void h2o_nif_ipc_unlink_ostream(h2o_nif_ipc_ostream_t *ostream);
extern void h2o_nif_ipc_link_request(h2o_nif_ipc_request_t *request, h2o_req_t *req, h2o_nif_ipc_receiver_cb_t *cb, h2o_nif_ipc_receiver_dtor_t *dtor);
extern void h2o_nif_ipc_unlink_request(h2o_nif_ipc_request_t *request);

#endif
