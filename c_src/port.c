// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "port.h"

// typedef struct h2o_nif_port_ctx_s {
//     h2o_linklist_t _link;
//     h2o_nif_port_t *port;
// } h2o_nif_port_ctx_t;

// ERL_NIF_TERM
// h2o_nif_port_mem_info(ErlNifEnv *env)
// {
//     ERL_NIF_TERM out;
//     // ERL_NIF_TERM port_ids;
//     // port_ids = enif_make_list(env, 0);
//     // (void)enif_rwlock_rlock(h2o_nif_rwlock);
//     // h2o_nif_port_t *port = NULL;
//     // kh_foreach_value(h2o_nif_ports, port, {
//     //     port_ids = enif_make_list_cell(env, enif_make_int(env, port->id), port_ids);
//     // });
//     // (void)enif_rwlock_runlock(h2o_nif_rwlock);
//     return out;
// }

h2o_nif_port_t *
h2o_nif_port_alloc(ErlNifEnv *env)
{
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *port_type = priv_data->port;
    h2o_nif_port_t *port = (h2o_nif_port_t *)enif_alloc_resource(port_type, sizeof(*port));
    if (port == NULL) {
        return NULL;
    }
    (void)memset(port, 0, sizeof(*port));
    __sync_fetch_and_add(&h2o_nif_num_ports, 1);
    {
        khiter_t iter;
        int r;
        // h2o_nif_port_ctx_t *ctx = enif_alloc(sizeof(*ctx));
        // (void)memset(ctx, 0, sizeof(*ctx));
        // ctx->port = port;
        do {
            port->id = __sync_fetch_and_add(&h2o_nif_seq_ports, 1);
            (void)enif_rwlock_rwlock(h2o_nif_rwlock);
            iter = kh_put(h2o_nif_port_t, h2o_nif_ports, port->id, &r);
            switch (r) {
            case -1: // the operation failed
                TRACE_F("khash: the operation failed\n");
                (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
                goto error;
            case 0: // the key is present in the hash table
                TRACE_F("khash: the key is present in the hash table\n");
                (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
                break;
            case 1: // the bucket is empty (never used)
                // TRACE_F("khash: the bucket is empty (never used)\n");
                kh_val(h2o_nif_ports, iter) = port;
                (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
                break;
            case 2: // the element in the bucket has been deleted
                // TRACE_F("khash: the element in the bucket has been deleted\n");
                kh_val(h2o_nif_ports, iter) = port;
                (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
                break;
            default:
                TRACE_F("khash: unknown error (%d)\n", r);
                (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
                goto error;
            }
        } while (r < 1);
    }
    // TRACE_F("open port: %p\n", port);
    return port;

error:
    port->state = H2O_NIF_PORT_STATE_CLOSED;
    port->opened = 1;
    port->closed = 1;
    port->released = 1;
    port->release_count = 1;
    (void)h2o_nif_port_release(port);
    return NULL;
}

h2o_nif_port_t *
h2o_nif_port_create(ErlNifEnv *env, h2o_nif_port_t *parent)
{
    h2o_nif_port_t *port = h2o_nif_port_alloc(env);
    if (port == NULL) {
        return NULL;
    }
    port->rwlock = enif_rwlock_create("h2o_port_rwlock");
    port->parent = parent;
    (void)h2o_linklist_init_anchor(&port->children);
    // if (parent == NULL) {
    //     port->env = enif_alloc_env();
    // } else {
    //     port->env = parent->env;
    // }
    // port->tag = enif_make_ref(port->env);
    // port->id = enif_make_tuple2(port->env, port->tag, enif_make_resource(port->env, (void *)port));
    (void)enif_self(env, &port->owner);
    port->state = H2O_NIF_PORT_STATE_OPEN;
    port->active = 0;
    port->active_count = 0;
    port->opened = 0;
    port->closed = 0;
    port->released = 0;
    port->release_count = 0;
    port->type = H2O_NIF_PORT_TYPE_NONE;
    port->on_close = NULL;
    port->data = NULL;
    if (parent != NULL) {
        __sync_fetch_and_add(&port->release_count, 1);
        (void)enif_keep_resource((void *)port);
        (void)h2o_linklist_insert(&parent->children, &port->_link);
    }
    // port->acc.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&port->acc.pending);
    // port->req.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&port->req.pending);
    // port->acc.mutex = enif_mutex_create("h2o_port_acc_
    return port;
}

int
h2o_nif_port_get(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp)
{
    int arity = 0;
    const ERL_NIF_TERM *tuple = NULL;
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *port_type = priv_data->port;
    h2o_nif_port_t *port = NULL;
    int port_id = 0;

    if (portp != NULL) {
        *portp = NULL;
    }

    if (enif_is_number(env, id)) {
        if (!h2o_nif_port_get_by_id(env, id, &port)) {
            return 0;
        }
    } else if (!enif_get_tuple(env, id, &arity, &tuple) || arity != 3 || tuple[0] != ATOM_h2o_port ||
               !enif_is_number(env, tuple[1]) || !enif_get_resource(env, tuple[2], port_type, (void **)&port) ||
               !enif_get_int(env, tuple[1], &port_id) || port->id != port_id) {
        return 0;
    }

    if (portp != NULL) {
        *portp = port;
    }

    return 1;
}

int
h2o_nif_port_get_by_id(ErlNifEnv *env, ERL_NIF_TERM id, h2o_nif_port_t **portp)
{
    h2o_nif_port_t *port = NULL;
    int port_id = 0;
    khiter_t iter;

    if (portp != NULL) {
        *portp = NULL;
    }

    if (!enif_get_int(env, id, &port_id)) {
        return 0;
    }

    (void)enif_rwlock_rlock(h2o_nif_rwlock);
    iter = kh_get(h2o_nif_port_t, h2o_nif_ports, port_id);
    if (iter == kh_end(h2o_nif_ports)) {
        (void)enif_rwlock_runlock(h2o_nif_rwlock);
        return 0;
    }
    port = kh_val(h2o_nif_ports, iter);
    if (port->closed) {
        port = NULL;
    }
    (void)enif_rwlock_runlock(h2o_nif_rwlock);

    if (port == NULL) {
        return 0;
    }

    if (portp != NULL) {
        *portp = port;
    }

    return 1;
}

int
h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port)
{
    int in_nif_call = (env == NULL) ? 0 : 1;
    // Send closed message if not already sent
    if (H2O_NIF_PORT_IS_OPEN(port)) {
        if (port->on_close != NULL) {
            (void)port->on_close(env, port);
        }
        port->state = H2O_NIF_PORT_STATE_CLOSED;
        ErlNifEnv *msg_env = (in_nif_call) ? env : enif_alloc_env();
        ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
        int msg_sent;
        if (in_nif_call) {
            msg_sent = enif_send(msg_env, &port->owner, NULL, msg);
        } else {
            msg_sent = enif_send(NULL, &port->owner, msg_env, msg);
        }
        if (!msg_sent && !in_nif_call) {
            (void)enif_free_env(msg_env);
        }
        // port->tag = 0;
        // port->id = 0;
    }
    // Unlink from parent, if present
    if (port->parent != NULL) {
        // TRACE_F("about to unlink parent\n");
        if (h2o_nif_port_rwlock(port->parent)) {
            (void)h2o_linklist_unlink(&port->_link);
            (void)h2o_nif_port_rwunlock(port->parent);
        }
        port->parent = NULL;
        // port->env = NULL;
    }
    // Close all children, if present
    if (!h2o_linklist_is_empty(&port->children)) {
        // TRACE_F("unlink children here\n");
        h2o_linklist_t *node = port->children.next;
        h2o_nif_port_t *child = NULL;
        while (node != &port->children) {
            child = (h2o_nif_port_t *)node;
            node = node->next;
            if (!h2o_nif_port_tryclose(child)) {
                continue;
            }
            child->parent = NULL;
            (void)h2o_linklist_unlink(&child->_link);
            (void)h2o_nif_port_close(env, child);
            (void)h2o_nif_port_rwunlock(child);
            if (!h2o_nif_port_tryrelease(child)) {
                continue;
            }
            (void)h2o_nif_port_release(child);
        }
    }
    // // Free the resource, if needed
    // if (port->needs_release) {
    //     port->needs_release
    // }
    // // Free the term environment, if present
    // if (port->env != NULL) {
    //     (void)enif_free_env(port->env);
    //     port->env = NULL;
    // }
    return 1;
}

// #include "h2o_nif/refc.c.h"

int
h2o_nif_port_release(h2o_nif_port_t *port)
{
    if (port == NULL) {
        return 0;
    }
    // TRACE_F("[%p] refc == %d\n", port, h2o_nif_port_refc(port));
    int release_count = 0;
    while (1) {
        release_count = port->release_count;
        if (release_count == 0) {
            break;
        }
        if (release_count == __sync_val_compare_and_swap(&port->release_count, release_count, 0)) {
            break;
        }
    }
    // TRACE_F("about to release %d times: %d\n", release_count, port->id);
    while (release_count--) {
        (void)enif_release_resource((void *)port);
    }
    // TRACE_F("[%p] refc -> %d\n", port, h2o_nif_port_refc(port));
    return 1;
}

void
h2o_nif_port_dtor(ErlNifEnv *env, void *obj)
{
    (void)enif_mutex_lock(h2o_nif_mutex);
    // TRACE_F("h2o_nif_port_dtor:%s:%d\n", __FILE__, __LINE__);
    // if (obj == NULL) {
    //     return;
    // }
    h2o_nif_port_t *port = (h2o_nif_port_t *)obj;
    // TRACE_F("is closed? %d\n", port->closed);
    (void)enif_mutex_unlock(h2o_nif_mutex);
    // (void)enif_rwlock_rwlock(port->rwlock);
    // (void)h2o_nif_port__close(NULL, port);
    // (void)enif_rwlock_rwunlock(port->rwlock);
    // (void)enif_rwlock_destroy(port->rwlock);
    // port->rwlock = NULL;
    if (h2o_nif_port_tryclose(port)) {
        // TRACE_F("closing port in dtor %d\n", port->id);
        (void)h2o_nif_port_close(NULL, port);
        (void)h2o_nif_port_rwunlock(port);
    }
    __sync_fetch_and_sub(&h2o_nif_num_ports, 1);
    {
        khiter_t iter;
        (void)enif_rwlock_rwlock(h2o_nif_rwlock);
        iter = kh_get(h2o_nif_port_t, h2o_nif_ports, port->id);
        if (iter != kh_end(h2o_nif_ports)) {
            kh_del(h2o_nif_port_t, h2o_nif_ports, iter);
        }
        (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
    }
    return;
}
