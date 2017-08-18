// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_RECORDS_H
#define H2O_NIF_RECORDS_H

#include "globals.h"
#include "port.h"

/* Types */

typedef struct h2o_nif_proto_req_s h2o_nif_proto_req_t;
typedef struct h2o_nif_proto_kv_s h2o_nif_proto_kv_t;

struct h2o_nif_proto_kv_s {
    ErlNifBinary key;
    ErlNifBinary val;
};

typedef H2O_VECTOR(h2o_nif_proto_kv_t) h2o_nif_proto_kvs_t;

struct h2o_nif_proto_req_s {
    h2o_mem_pool_t pool;
    h2o_nif_port_t *event;
    int event_type;
    ErlNifBinary authority;
    size_t body_length;
    bool has_body;
    bool has_read_body;
    bool has_sent_resp;
    h2o_nif_proto_kvs_t headers;
    ErlNifBinary host;
    ErlNifBinary method;
    ErlNifBinary path;
    struct {
        ErlNifBinary address;
        int32_t port;
    } peer;
    int32_t port;
    struct {
        int status;
        ErlNifBinary reason;
        size_t content_length;
        h2o_nif_proto_kvs_t headers;
    } res;
    ErlNifBinary scheme;
    h2o_send_state_t send_state;
    uint32_t streamid;
    int version;
};

// struct h2o_nif_record_res_s {
//     ERL_NIF_TERM status;
//     ERL_NIF_TERM reason;
//     ERL_NIF_TERM content_length;
//     ERL_NIF_TERM headers;
// };

// struct h2o_nif_record_req_s {
//     ErlNifEnv *env;
//     ERL_NIF_TERM event;
//     ERL_NIF_TERM event_type;
//     ERL_NIF_TERM authority;
//     ERL_NIF_TERM body_length;
//     ERL_NIF_TERM has_body;
//     ERL_NIF_TERM has_read_body;
//     ERL_NIF_TERM has_sent_resp;
//     ERL_NIF_TERM headers;
//     ERL_NIF_TERM host;
//     ERL_NIF_TERM method;
//     ERL_NIF_TERM multipart;
//     ERL_NIF_TERM path;
//     ERL_NIF_TERM peer;
//     ERL_NIF_TERM port;
//     h2o_nif_record_res_t res;
//     ERL_NIF_TERM resp_body;
//     ERL_NIF_TERM resp_cookies;
//     ERL_NIF_TERM resp_headers;
//     ERL_NIF_TERM scheme;
//     ERL_NIF_TERM send_state;
//     ERL_NIF_TERM streamid;
//     ERL_NIF_TERM version;
// };

/* Record Functions */

extern int h2o_nif_proto_req_init(h2o_nif_port_t *port, h2o_nif_proto_req_t *proto);
extern ERL_NIF_TERM h2o_nif_proto_req_make(ErlNifEnv *env, h2o_nif_proto_req_t *proto);

// extern int h2o_nif_record_req_init(h2o_nif_port_t *port, h2o_nif_record_req_t *rec);
// extern void h2o_nif_record_req_dispose(h2o_nif_record_req_t *rec);
// extern ERL_NIF_TERM h2o_nif_record_req_make(ErlNifEnv *env, h2o_nif_record_req_t *rec);

#endif
