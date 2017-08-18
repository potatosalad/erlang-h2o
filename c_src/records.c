// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "records.h"

#include <h2o.h>
#include <h2o/configurator.h>
#include <h2o/http1.h>
#include <h2o/http2.h>
#include <h2o/http2_internal.h>
#include <h2o/serverutil.h>

#include "filter_event.h"

/* Record Functions */

static void h2o_nif_proto_req_copy(h2o_req_t *req, h2o_nif_proto_req_t *proto);
static ErlNifBinary iovec_to_binary(h2o_iovec_t iov);
static void headers_to_kvs(h2o_mem_pool_t *pool, h2o_headers_t *headers, h2o_nif_proto_kvs_t *kvs);
static ERL_NIF_TERM kvs_to_list(ErlNifEnv *env, h2o_nif_proto_kvs_t *kvs);
static ERL_NIF_TERM version_to_atom(int version);

int
h2o_nif_proto_req_init(h2o_nif_port_t *port, h2o_nif_proto_req_t *proto)
{
    h2o_req_t *req = NULL;
    if (port->type == H2O_NIF_PORT_TYPE_FILTER_EVENT) {
        h2o_nif_filter_event_t *filter_event = (h2o_nif_filter_event_t *)port;
        req = filter_event->ostream.req;
    } else {
        return 0;
    }
    (void)h2o_mem_init_pool(&proto->pool);
    proto->event = port;
    proto->event_type = port->type;
    (void)h2o_nif_proto_req_copy(req, proto);
    return 1;
}

// void
// h2o_nif_record_req_dispose(h2o_nif_record_req_t *rec)
// {
//     if (rec->env != NULL) {
//         (void)enif_free_env(rec->env);
//     }
//     (void)memset(rec, 0, sizeof(*rec));
// }

ERL_NIF_TERM
h2o_nif_proto_req_make(ErlNifEnv *env, h2o_nif_proto_req_t *proto)
{
    TRACE_F("h2o_nif_proto_req_make:%s:%d\n", __FILE__, __LINE__);
    ERL_NIF_TERM term;
    ERL_NIF_TERM tuple[23];
    size_t i = 0;
    if (proto->event == NULL) {
        return enif_make_badarg(env);
    }
    tuple[i++] = ATOM_h2o_req;
    tuple[i++] = h2o_nif_port_make(env, proto->event);
    tuple[i++] = (proto->event_type == H2O_NIF_PORT_TYPE_FILTER_EVENT) ? ATOM_filter : ATOM_undefined;
    // DEBUG_F("enif_make_binary(env, &proto->authority) -> %llu\n", proto->authority.size);
    tuple[i++] = enif_make_binary(env, &proto->authority);
    tuple[i++] = enif_make_ulong(env, proto->body_length);
    tuple[i++] = (proto->has_body == true) ? ATOM_true : ATOM_false;
    tuple[i++] = (proto->has_read_body == true) ? ATOM_true : ATOM_false;
    tuple[i++] = (proto->has_sent_resp == true) ? ATOM_true : ATOM_false;
    tuple[i++] = kvs_to_list(env, &proto->headers);
    tuple[i++] = enif_make_binary(env, &proto->host);
    tuple[i++] = enif_make_binary(env, &proto->method);
    tuple[i++] = ATOM_undefined;
    tuple[i++] = enif_make_binary(env, &proto->path);
    if (proto->peer.port == -1) {
        tuple[i++] = ATOM_undefined;
    } else {
        // DEBUG_F("enif_make_binary(env, &proto->peer.address) -> %llu\n", proto->peer.address.size);
        tuple[i++] = enif_make_tuple2(env, enif_make_binary(env, &proto->peer.address), enif_make_long(env, proto->peer.port));
    }
    tuple[i++] = enif_make_long(env, proto->port);
    {
        ERL_NIF_TERM res[5];
        size_t j = 0;
        res[j++] = ATOM_h2o_res;
        res[j++] = enif_make_int(env, proto->res.status);
        // DEBUG_F("enif_make_binary(env, &proto->res.reason) -> %llu\n", proto->res.reason.size);
        res[j++] = enif_make_binary(env, &proto->res.reason);
        res[j++] = enif_make_ulong(env, proto->res.content_length);
        res[j++] = kvs_to_list(env, &proto->res.headers);
        tuple[i++] = enif_make_tuple_from_array(env, res, j);
    }
    tuple[i++] = ATOM_undefined;
    tuple[i++] = ATOM_undefined;
    tuple[i++] = ATOM_undefined;
    // DEBUG_F("enif_make_binary(env, &proto->scheme) -> %llu\n", proto->scheme.size);
    tuple[i++] = enif_make_binary(env, &proto->scheme);
    tuple[i++] = (proto->send_state == H2O_SEND_STATE_IN_PROGRESS) ? ATOM_in_progress : ATOM_finalized;
    tuple[i++] = enif_make_ulong(env, proto->streamid);
    tuple[i++] = version_to_atom(proto->version);
    term = enif_make_tuple_from_array(env, tuple, i);
    proto->event = NULL;
    (void)h2o_mem_clear_pool(&proto->pool);
    return term;
}

static void
h2o_nif_proto_req_copy(h2o_req_t *req, h2o_nif_proto_req_t *proto)
{
    h2o_http2_stream_t *stream = NULL;
    if (req->version >= 0x200) {
        stream = H2O_STRUCT_FROM_MEMBER(h2o_http2_stream_t, req, req);
    }
    /* authority */
    proto->authority = iovec_to_binary(req->authority);
    /* body_length */
    proto->body_length = (req->entity.base == NULL) ? 0 : req->entity.len;
    /* has_body */
    proto->has_body = (req->entity.base == NULL) ? false : true;
    /* has_read_body */
    proto->has_read_body = false;
    /* has_sent_resp */
    proto->has_sent_resp = false;
    /* headers */
    (void)headers_to_kvs(&proto->pool, &req->headers, &proto->headers);
    /* host */
    proto->host = iovec_to_binary(req->hostconf->authority.host);
    /* method */
    proto->method = iovec_to_binary(req->method);
    /* path */
    proto->path = iovec_to_binary(req->path);
    /* peer */
    {
        struct sockaddr_storage ss;
        socklen_t sslen;
        size_t remote_addr_len = SIZE_MAX;
        char remote_addr[NI_MAXHOST];
        proto->peer.address.data = NULL;
        proto->peer.address.size = 0;
        proto->peer.port = -1;
        if ((sslen = req->conn->callbacks->get_peername(req->conn, (void *)&ss)) != 0) {
            if ((remote_addr_len = h2o_socket_getnumerichost((void *)&ss, sslen, remote_addr)) != SIZE_MAX) {
                proto->peer.address = iovec_to_binary((h2o_iovec_t){remote_addr, remote_addr_len});
                proto->peer.port = h2o_socket_getport((void *)&ss);
            }
        }
    }
    /* port */
    proto->port = req->hostconf->authority.port;
    /* res */
    {
        /* status */
        proto->res.status = req->res.status;
        /* reason */
        if (req->res.reason == NULL) {
            proto->res.reason.data = NULL;
            proto->res.reason.size = 0;
        } else {
            proto->res.reason = iovec_to_binary((h2o_iovec_t){(char *)req->res.reason, strlen(req->res.reason)});
        }
        /* content_length */
        proto->res.content_length = req->res.content_length;
        /* headers */
        (void)headers_to_kvs(&proto->pool, &req->res.headers, &proto->res.headers);
    }
    /* scheme */
    proto->scheme = iovec_to_binary(req->scheme->name);
    /* send_state */
    proto->send_state = H2O_SEND_STATE_IN_PROGRESS;
    /* streamid */
    if (stream == NULL) {
        proto->streamid = 0;
    } else {
        proto->streamid = stream->stream_id;
    }
    /* version */
    proto->version = req->version;
}

static ErlNifBinary
iovec_to_binary(h2o_iovec_t iov)
{
    ErlNifBinary binary;
    // DEBUG_F("enif_alloc_binary(%lu):%s:%d\n", iov.len, __FILE__, __LINE__);
    if (!enif_alloc_binary(iov.len, &binary)) {
        perror("unable to allocate iovec binary");
        abort();
    }
    (void)memcpy(binary.data, iov.base, iov.len);
    return binary;
}

static void
headers_to_kvs(h2o_mem_pool_t *pool, h2o_headers_t *headers, h2o_nif_proto_kvs_t *kvs)
{
    size_t i;
    (void)h2o_vector_reserve(pool, kvs, headers->size);
    for (i = 0; i < headers->size; i++) {
        kvs->entries[i].key = iovec_to_binary(*(headers->entries[i].name));
        kvs->entries[i].val = iovec_to_binary(headers->entries[i].value);
    }
}

static ERL_NIF_TERM
kvs_to_list(ErlNifEnv *env, h2o_nif_proto_kvs_t *kvs)
{
    ERL_NIF_TERM list[kvs->size];
    size_t i;
    for (i = 0; i < kvs->size; i++) {
        // DEBUG_F("enif_make_binary(env, &kvs->entries[i].key) -> %llu\n", &kvs->entries[i].key.size);
        // DEBUG_F("enif_make_binary(env, &kvs->entries[i].val) -> %llu\n", &kvs->entries[i].val.size);
        list[i] = enif_make_tuple2(env, enif_make_binary(env, &kvs->entries[i].key), enif_make_binary(env, &kvs->entries[i].val));
    }
    return enif_make_list_from_array(env, list, i);
}

inline ERL_NIF_TERM
version_to_atom(int version)
{
    if (version < 0x200) {
        assert(version <= 0x109);
        if ((version & 0xff) == 0x00) {
            return ATOM_HTTP_1_0;
        }
        return ATOM_HTTP_1_1;
    }
    return ATOM_HTTP_2;
}
