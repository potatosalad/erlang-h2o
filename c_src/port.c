// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "port.h"

ck_spinlock_t h2o_nif_ports_spinlock;

#define H2O_NIF_PORT_GC_MAX 100

static _Atomic uintptr_t port_seq = ATOMIC_VAR_INIT(0);
static ErlNifResourceType *port_type = NULL;
static _Atomic unsigned long port_async_seq = ATOMIC_VAR_INIT(0);
static ck_spinlock_t port_gc_spinlock = CK_SPINLOCK_INITIALIZER;
static ErlNifTid port_gc_tid;
static _Atomic unsigned long long port_gc_min = ATOMIC_VAR_INIT(0);
static _Atomic unsigned long long port_gc_max = ATOMIC_VAR_INIT(0);
static _Atomic unsigned long long port_gc_avg = ATOMIC_VAR_INIT(0);

int
h2o_nif_ports_load(h2o_nif_data_t *nif_data)
{
    port_type = nif_data->port;
    (void)ck_spinlock_init(&h2o_nif_ports_spinlock);
    return 1;
}

void
h2o_nif_ports_unload(void)
{
    port_type = NULL;
    return;
}

int
h2o_nif_ports_stat(h2o_nif_ports_stat_t *stat)
{
    if (stat != NULL) {
        stat->gc_avg = atomic_load_explicit(&port_gc_avg, memory_order_relaxed);
        stat->gc_max = atomic_load_explicit(&port_gc_max, memory_order_relaxed);
        stat->gc_min = atomic_load_explicit(&port_gc_min, memory_order_relaxed);
        stat->seq = atomic_load_explicit(&port_seq, memory_order_relaxed);
    }
    return 1;
}

h2o_nif_port_t *
h2o_nif_port_alloc(void)
{
    ErlNifResourceType *resource_type = port_type;
    if (resource_type == NULL) {
        return NULL;
    }
    h2o_nif_port_t *port = (h2o_nif_port_t *)enif_alloc_resource(resource_type, sizeof(*port));
    if (port == NULL) {
        return NULL;
    }
    (void)memset(port, 0, sizeof(*port));
    ck_fifo_mpmc_entry_t *stub[1];
    stub[0] = (ck_fifo_mpmc_entry_t *)malloc(sizeof(ck_fifo_mpmc_entry_t));
    if (stub[0] == NULL) {
        goto error;
    }
    {
        int r = 0;
        do {
            port->id = atomic_fetch_add_explicit(&port_seq, 1, memory_order_relaxed);
            r = h2o_nif_hm_put(port->id, (uintptr_t)port);
        } while (r == 0);
        if (r != 1) {
            if (stub[0] != NULL) {
                (void)free(stub[0]);
                stub[0] = NULL;
            }
            goto error;
        }
    }
    (void)atomic_init(&port->state, H2O_NIF_PORT_STATE_ALLOCATED);
    (void)atomic_init(&port->active, H2O_NIF_PORT_ACTIVE_FALSE);
    (void)atomic_init(&port->num_output, 0);
    (void)atomic_init(&port->num_accept, 0);
    (void)atomic_init(&port->num_listen, 0);
    (void)ck_spinlock_init(&port->spinlock_accept);
    (void)ck_fifo_mpmc_init(&port->output, stub[0]);
    (void)h2o_linklist_init_anchor(&port->accept);
    (void)h2o_linklist_init_anchor(&port->listen);
    port->has_garbage = 1;
    (void)h2o_nif_port_keep(port);
    return port;

error:
    (void)atomic_init(&port->state, H2O_NIF_PORT_STATE_CLOSED);
    (void)enif_release_resource((void *)port);
    return NULL;
}

// static H2O_NIF_RETURN
// fake_close(ErlNifEnv *env, h2o_nif_port_t *port, h2o_nif_return_ctx_t *ctx)
// {
//     if (ctx->data == NULL) {
//         TRACE_F("data is NULL\n");
//         int *x = (int *)enif_alloc(sizeof(*x));
//         *x = 1;
//         ctx->data = (void *)x;
//         return H2O_NIF_RETURN_TRAP;
//     }
//     int *x = (int *)ctx->data;
//     TRACE_F("x is %d\n", *x);
//     (*x)++;
//     if ((*x) == 10) {
//         ctx->data = NULL;
//         (void)enif_free((void *)x);
//         return H2O_NIF_RETURN_DONE;
//     }
//     return H2O_NIF_RETURN_TRAP;
// }

int
h2o_nif_port_open(h2o_nif_port_t *parent, h2o_nif_port_t **portp)
{
    assert(portp != NULL);
    h2o_nif_port_t *port = h2o_nif_port_alloc();
    if (port == NULL) {
        *portp = NULL;
        return 0;
    }
    port->parent = NULL;
    if (parent != NULL) {
        (void)h2o_nif_port_keep(parent);
        port->parent = parent;
    }
    (void)h2o_linklist_init_anchor(&port->children);
    (void)atomic_init(&port->num_children, 0);
    if (parent != NULL) {
        (void)h2o_nif_port_keep(port);
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        (void)h2o_linklist_insert(&parent->children, &port->_link);
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        (void)atomic_fetch_add_explicit(&parent->num_children, 1, memory_order_relaxed);
    }
    port->on_close.state = 0;
    port->on_close.callback = NULL;
    // port->on_close.callback = fake_close;
    port->on_close.data = NULL;
    port->on_close.out = ATOM_ok;
    port->on_accept = NULL;
    port->type = H2O_NIF_PORT_TYPE_NONE;
    port->data = NULL;
    *portp = port;
    return 1;
    // port->rwlock = enif_rwlock_create("h2o_port_rwlock");
    // port->parent = parent;
    // (void)h2o_linklist_init_anchor(&port->children);
    // if (parent == NULL) {
    //     port->env = enif_alloc_env();
    // } else {
    //     port->env = parent->env;
    // }
    // port->tag = enif_make_ref(port->env);
    // port->id = enif_make_tuple2(port->env, port->tag, enif_make_resource(port->env, (void *)port));
    // (void)enif_self(env, &port->owner);
    // port->state = H2O_NIF_PORT_STATE_OPEN;
    // port->active = 0;
    // port->active_count = 0;
    // port->opened = 0;
    // port->closed = 0;
    // port->released = 0;
    // port->release_count = 0;
    // port->type = H2O_NIF_PORT_TYPE_NONE;
    // port->on_close = NULL;
    // port->data = NULL;
    // if (parent != NULL) {
    //     __sync_fetch_and_add(&port->release_count, 1);
    //     (void)enif_keep_resource((void *)port);
    //     (void)h2o_linklist_insert(&parent->children, &port->_link);
    // }
    // port->acc.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&port->acc.pending);
    // port->req.mutex = NULL;
    // (void)h2o_linklist_init_anchor(&port->req.pending);
    // port->acc.mutex = enif_mutex_create("h2o_port_acc_
    // return port;
}

int
h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port, H2O_NIF_RETURN *nif_return)
{
    int state = h2o_nif_port_fetch_set_state(port, H2O_NIF_PORT_STATE_CLOSED);
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        // Already closed by another thread
        return 0;
    }
    port->on_close.state = state;
    return _h2o_nif_port_close(env, port, nif_return);
}

static void
_h2o_nif_port_close_immediate(ErlNifEnv *env, h2o_nif_port_t *port)
{
    // This function is unsafe as-is, but here's an extra safety check
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        return;
    }
    // Run close callback if present
    if (port->on_close.callback != NULL) {
        H2O_NIF_RETURN on_close = port->on_close.callback(env, port, &port->on_close);
        while (on_close == H2O_NIF_RETURN_TRAP) {
            (void)ck_pr_stall();
            on_close = port->on_close.callback(env, port, &port->on_close);
        }
    }
    return;
}

int
_h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port, H2O_NIF_RETURN *nif_return)
{
    bool is_nif_call = (env != NULL);
    // This function is unsafe as-is, but here's an extra safety check
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        return 0;
    }
    // Run close callback if present
    if (port->on_close.callback != NULL) {
        H2O_NIF_RETURN on_close = port->on_close.callback(env, port, &port->on_close);
        if (on_close == H2O_NIF_RETURN_TRAP) {
            if (is_nif_call && nif_return != NULL) {
                *nif_return = H2O_NIF_RETURN_TRAP;
                return 1;
            }
            (void)_h2o_nif_port_close_immediate(env, port);
        }
        // Finished with callback
        port->on_close.callback = NULL;
    }
    // Send closed message to owner if port was open
    if ((port->on_close.state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) {
        ErlNifEnv *msg_env = (is_nif_call) ? env : enif_alloc_env();
        ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
        ErlNifPid owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
        if (is_nif_call) {
            (void)enif_send(msg_env, &owner, NULL, msg);
        } else {
            (void)enif_send(NULL, &owner, msg_env, msg);
            (void)enif_free_env(msg_env);
        }
    }
    // Unlink from parent, if present
    {
        h2o_nif_port_t *parent = NULL;
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        parent = port->parent;
        if (parent != NULL) {
            (void)h2o_linklist_unlink(&port->_link);
            port->parent = NULL;
            (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
            (void)atomic_fetch_sub_explicit(&parent->num_children, 1, memory_order_relaxed);
            (void)h2o_nif_port_release(parent);
            (void)h2o_nif_port_release(port);
        } else {
            (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        }
    }
    // Close all children, if present
    {
        h2o_linklist_t *anchor = NULL;
        h2o_linklist_t *node = NULL;
        h2o_nif_port_t *child = NULL;
        h2o_linklist_t garbage;
        int num_garbage = 0;
        (void)h2o_linklist_init_anchor(&garbage);
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        anchor = &port->children;
        node = anchor->next;
        while (node != anchor) {
            child = (h2o_nif_port_t *)node;
            node = node->next;
            (void)h2o_nif_port_keep(child);
            if (!h2o_nif_port_is_closed(child)) {
                (void)h2o_linklist_unlink(&child->_link);
                child->parent = NULL;
                // Defer release until later
                (void)h2o_linklist_insert(&garbage, &child->_link);
                num_garbage++;
            } else {
                // Child already closing in progress by another thread, allow it to unlink itself
                (void)h2o_nif_port_release(child);
            }
        }
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        (void)atomic_fetch_sub_explicit(&port->num_children, num_garbage, memory_order_relaxed);
        // TRACE_F("num_children=%d, num_garbage=%d\n", atomic_load_explicit(&port->num_children, memory_order_relaxed),
        // num_garbage);
        anchor = &garbage;
        node = anchor->next;
        while (node != anchor) {
            child = (h2o_nif_port_t *)node;
            node = node->next;
            (void)h2o_nif_port_close(env, child, NULL);
            // Deferred release from above
            (void)h2o_nif_port_release(child);
            (void)h2o_nif_port_release(child);
            (void)h2o_nif_port_release(port);
        }
    }
    // Remove port from hashmap
    (void)h2o_nif_hm_del(port->id);
    (void)h2o_nif_port_release(port);
    // Final release (cancels out allocation)
    (void)h2o_nif_port_release(port);
    // Set the nif_return to done if present
    if (nif_return != NULL) {
        *nif_return = H2O_NIF_RETURN_DONE;
    }
    return 1;
}

// int
// h2o_nif_port_close(ErlNifEnv *env, h2o_nif_port_t *port)
// {
//     int in_nif_call = (env == NULL) ? 0 : 1;
//     // Send closed message if not already sent
//     if (H2O_NIF_PORT_IS_OPEN(port)) {
//         if (port->on_close != NULL) {
//             (void)port->on_close(env, port);
//         }
//         port->state = H2O_NIF_PORT_STATE_CLOSED;
//         ErlNifEnv *msg_env = (in_nif_call) ? env : enif_alloc_env();
//         ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
//         int msg_sent;
//         if (in_nif_call) {
//             msg_sent = enif_send(msg_env, &port->owner, NULL, msg);
//         } else {
//             msg_sent = enif_send(NULL, &port->owner, msg_env, msg);
//         }
//         if (!msg_sent && !in_nif_call) {
//             (void)enif_free_env(msg_env);
//         }
//         // port->tag = 0;
//         // port->id = 0;
//     }
//     // Unlink from parent, if present
//     if (port->parent != NULL) {
//         // TRACE_F("about to unlink parent\n");
//         if (h2o_nif_port_rwlock(port->parent)) {
//             (void)h2o_linklist_unlink(&port->_link);
//             (void)h2o_nif_port_rwunlock(port->parent);
//         }
//         port->parent = NULL;
//         // port->env = NULL;
//     }
//     // Close all children, if present
//     if (!h2o_linklist_is_empty(&port->children)) {
//         // TRACE_F("unlink children here\n");
//         h2o_linklist_t *node = port->children.next;
//         h2o_nif_port_t *child = NULL;
//         while (node != &port->children) {
//             child = (h2o_nif_port_t *)node;
//             node = node->next;
//             if (!h2o_nif_port_tryclose(child)) {
//                 continue;
//             }
//             child->parent = NULL;
//             (void)h2o_linklist_unlink(&child->_link);
//             (void)h2o_nif_port_close(env, child);
//             (void)h2o_nif_port_rwunlock(child);
//             if (!h2o_nif_port_tryrelease(child)) {
//                 continue;
//             }
//             (void)h2o_nif_port_release(child);
//         }
//     }
//     // // Free the resource, if needed
//     // if (port->needs_release) {
//     //     port->needs_release
//     // }
//     // // Free the term environment, if present
//     // if (port->env != NULL) {
//     //     (void)enif_free_env(port->env);
//     //     port->env = NULL;
//     // }
//     return 1;
// }

// #include "h2o_nif/refc.c.h"

// int
// h2o_nif_port_release(h2o_nif_port_t *port)
// {
//     if (port == NULL) {
//         return 0;
//     }
//     // TRACE_F("[%p] refc == %d\n", port, h2o_nif_port_refc(port));
//     int release_count = 0;
//     while (1) {
//         release_count = port->release_count;
//         if (release_count == 0) {
//             break;
//         }
//         if (release_count == __sync_val_compare_and_swap(&port->release_count, release_count, 0)) {
//             break;
//         }
//     }
//     // TRACE_F("about to release %d times: %d\n", release_count, port->id);
//     while (release_count--) {
//         (void)enif_release_resource((void *)port);
//     }
//     // TRACE_F("[%p] refc -> %d\n", port, h2o_nif_port_refc(port));
//     return 1;
// }

void
h2o_nif_port_dtor(ErlNifEnv *env, void *obj)
{
    // (void)enif_mutex_lock(h2o_nif_mutex);
    TRACE_F("h2o_nif_port_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = (h2o_nif_port_t *)obj;
    if (port->has_garbage) {
        ck_fifo_mpmc_entry_t *garbage[1];
        (void)ck_fifo_mpmc_deinit(&port->output, &garbage[0]);
        // assert((garbage[0] == garbage[1]) && (garbage[1] == garbage[2]));
        (void)free(garbage[0]);
        port->has_garbage = 0;
    }
    if (!h2o_nif_port_is_closed(port)) {
        TRACE_F("SHOULD NEVER HAPPEN: port dtor called for non-closed port\n");
        perror("non-closed port dtor:");
        exit(1);
    }
    // (void)h2o_nif_hm_del(port->id);
    // if (obj == NULL) {
    //     return;
    // }
    // h2o_nif_port_t *port = (h2o_nif_port_t *)obj;
    // // TRACE_F("is closed? %d\n", port->closed);
    // (void)enif_mutex_unlock(h2o_nif_mutex);
    // // (void)enif_rwlock_rwlock(port->rwlock);
    // // (void)h2o_nif_port__close(NULL, port);
    // // (void)enif_rwlock_rwunlock(port->rwlock);
    // // (void)enif_rwlock_destroy(port->rwlock);
    // // port->rwlock = NULL;
    // if (h2o_nif_port_tryclose(port)) {
    //     // TRACE_F("closing port in dtor %d\n", port->id);
    //     (void)h2o_nif_port_close(NULL, port);
    //     (void)h2o_nif_port_rwunlock(port);
    // }
    // __sync_fetch_and_sub(&h2o_nif_num_ports, 1);
    // {
    //     khiter_t iter;
    //     (void)enif_rwlock_rwlock(h2o_nif_rwlock);
    //     iter = kh_get(h2o_nif_port_t, h2o_nif_ports, port->id);
    //     if (iter != kh_end(h2o_nif_ports)) {
    //         kh_del(h2o_nif_port_t, h2o_nif_ports, iter);
    //     }
    //     (void)enif_rwlock_rwunlock(h2o_nif_rwlock);
    // }
    return;
}

/* Listen/Accept Functions */

static inline int
h2o_nif_port_on_accept(h2o_nif_port_t *parent, h2o_nif_port_listen_t *listen, h2o_nif_port_t **portp)
{
    // TRACE_F("h2o_nif_port_on_accept A:%d\n", __LINE__);
    h2o_nif_port_t *child = NULL;
    int retval;
    assert(portp != NULL);
    if (parent->on_accept != NULL) {
        retval = parent->on_accept(parent, listen, &child);
        *portp = child;
        // TRACE_F("h2o_nif_port_on_accept B:%d\n", __LINE__);
        return retval;
    }
    retval = h2o_nif_port_open(parent, &child);
    (void)h2o_nif_port_cas_set_state(child, H2O_NIF_PORT_STATE_OPEN);
    // (void)enif_free(listen);
    *portp = child;
    // TRACE_F("h2o_nif_port_on_accept B:%d\n", __LINE__);
    return retval;
}

int
h2o_nif_port_listen_dispatch(h2o_nif_port_t *port, h2o_nif_port_listen_t *port_listen)
{
    // TRACE_F("h2o_nif_port_listen_dispatch A:%d\n", __LINE__);
    h2o_nif_port_accept_t *port_accept = NULL;
    h2o_nif_port_t *accept_port = NULL;
    ErlNifEnv *env = NULL;
    ERL_NIF_TERM msg;
    int dispatched;
    (void)gettimeofday(&port_listen->before_accept, NULL);
    do {
        dispatched = 0;
        (void)ck_spinlock_lock_eb(&port->spinlock_accept);
        if (h2o_linklist_is_empty(&port->accept)) {
            (void)h2o_linklist_insert(&port->listen, &port_listen->_link);
            (void)ck_spinlock_unlock(&port->spinlock_accept);
            (void)atomic_fetch_add_explicit(&port->num_listen, 1, memory_order_relaxed);
            TRACE_F("h2o_nif_port_listen_dispatch B:%d\n", __LINE__);
            return 1;
        }
        assert(h2o_linklist_is_empty(&port->listen));
        port_accept = (h2o_nif_port_accept_t *)port->accept.next;
        (void)h2o_linklist_unlink(&port_accept->_link);
        (void)ck_spinlock_unlock(&port->spinlock_accept);
        (void)atomic_fetch_sub_explicit(&port->num_accept, 1, memory_order_relaxed);
        env = port_accept->msg_env;
        if (accept_port == NULL) {
            assert(h2o_nif_port_on_accept(port, port_listen, &accept_port));
        }
        (void)h2o_nif_port_set_owner(accept_port, port_accept->caller);
        msg = enif_make_tuple3(env, ATOM_accept, enif_make_ulong(env, port_accept->id), h2o_nif_port_make(env, accept_port));
        dispatched = enif_send(NULL, &port_accept->caller, env, msg);
        (void)enif_free_env(port_accept->msg_env);
        (void)enif_free(port_accept);
    } while (dispatched == 0);
    if (dispatched) {

    }
    // TRACE_F("h2o_nif_port_listen_dispatch B:%d\n", __LINE__);
    return dispatched;
}

int
h2o_nif_port_accept(ErlNifEnv *env, h2o_nif_port_t *port, ERL_NIF_TERM *out)
{
    // TRACE_F("h2o_nif_port_accept A:%d\n", __LINE__);
    h2o_nif_port_accept_t *port_accept = NULL;
    h2o_nif_port_listen_t *port_listen = NULL;
    h2o_nif_port_t *accept_port = NULL;
    unsigned long async_id;
    ErlNifPid caller;
    // Fastpath accept if the listen queue is not empty.
    (void)ck_spinlock_lock_eb(&port->spinlock_accept);
    if (!h2o_linklist_is_empty(&port->listen)) {
        assert(h2o_linklist_is_empty(&port->accept));
        port_listen = (h2o_nif_port_listen_t *)port->listen.next;
        (void)h2o_linklist_unlink(&port_listen->_link);
        (void)ck_spinlock_unlock(&port->spinlock_accept);
        (void)atomic_fetch_sub_explicit(&port->num_listen, 1, memory_order_relaxed);
        assert(h2o_nif_port_on_accept(port, port_listen, &accept_port));
        (void)enif_self(env, &caller);
        (void)h2o_nif_port_set_owner(accept_port, caller);
        *out = enif_make_tuple2(env, ATOM_ok, h2o_nif_port_make(env, accept_port));
        // TRACE_F("h2o_nif_port_accept B:%d\n", __LINE__);
        return 1;
    }
    (void)ck_spinlock_unlock(&port->spinlock_accept);
    // Allocate accept for normal path.
    port_accept = (h2o_nif_port_accept_t *)enif_alloc(sizeof(h2o_nif_port_accept_t));
    if (port_accept == NULL) {
        // TRACE_F("h2o_nif_port_accept B:%d\n", __LINE__);
        return 0;
    }
    (void)memset(port_accept, 0, sizeof(h2o_nif_port_accept_t));
    port_accept->msg_env = enif_alloc_env();
    if (port_accept->msg_env == NULL) {
        (void)enif_free(port_accept);
        // TRACE_F("h2o_nif_port_accept B:%d\n", __LINE__);
        return 0;
    }
    async_id = port_accept->id = atomic_fetch_add_explicit(&port_async_seq, 1, memory_order_relaxed);
    (void)enif_self(env, &port_accept->caller);
    (void)ck_spinlock_lock_eb(&port->spinlock_accept);
    if (!h2o_linklist_is_empty(&port->listen)) {
        assert(h2o_linklist_is_empty(&port->accept));
        port_listen = (h2o_nif_port_listen_t *)port->listen.next;
        (void)h2o_linklist_unlink(&port_listen->_link);
        (void)ck_spinlock_unlock(&port->spinlock_accept);
        (void)atomic_fetch_sub_explicit(&port->num_listen, 1, memory_order_relaxed);
        assert(h2o_nif_port_on_accept(port, port_listen, &accept_port));
        (void)enif_self(env, &caller);
        (void)h2o_nif_port_set_owner(accept_port, caller);
        *out = enif_make_tuple2(env, ATOM_ok, h2o_nif_port_make(env, accept_port));
        (void)enif_free_env(port_accept->msg_env);
        (void)enif_free(port_accept);
        // TRACE_F("h2o_nif_port_accept B:%d\n", __LINE__);
        return 1;
    }
    (void)h2o_linklist_insert(&port->accept, &port_accept->_link);
    (void)ck_spinlock_unlock(&port->spinlock_accept);
    (void)atomic_fetch_add_explicit(&port->num_accept, 1, memory_order_relaxed);
    *out = enif_make_tuple2(env, ATOM_accept, enif_make_ulong(env, async_id));
    // TRACE_F("h2o_nif_port_accept B:%d\n", __LINE__);
    return 1;
}

/* Output Functions */

int
h2o_nif_port_output_reserve_send(h2o_nif_port_t *port)
{
    int expected;
    int desired;
    bool success;
    do {
        expected = atomic_load_explicit(&port->active, memory_order_relaxed);
        switch (expected) {
        case H2O_NIF_PORT_ACTIVE_FALSE:
            return H2O_NIF_PORT_ACTIVE_FALSE;
        case H2O_NIF_PORT_ACTIVE_TRUE:
            return H2O_NIF_PORT_ACTIVE_TRUE;
        case H2O_NIF_PORT_ACTIVE_ONCE:
            desired = H2O_NIF_PORT_ACTIVE_FALSE;
            success = atomic_compare_exchange_weak_explicit(&port->active, &expected, desired, memory_order_relaxed,
                                                            memory_order_relaxed);
            break;
        }
        if (!success) {
            (void)ck_pr_stall();
        }
    } while (!success);
    return expected;
}

int
h2o_nif_port_output_send(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    int active;
    int retval;
    if ((active = h2o_nif_port_output_reserve_send(port)) != H2O_NIF_PORT_ACTIVE_FALSE) {
        // TRACE_F("trying to send, reserved\n");
        ErlNifPid owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
        if (msg_env) {
            msg = enif_make_tuple3(msg_env, ATOM_h2o_port_data, h2o_nif_port_make(msg_env, port), msg);
        } else {
            msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, port), msg);
        }
        retval = enif_send(env, &owner, msg_env, msg);
        if (!retval && active == H2O_NIF_PORT_ACTIVE_ONCE) {
            retval = H2O_NIF_PORT_ACTIVE_FALSE;
            (void)atomic_compare_exchange_weak_explicit(&port->active, &retval, active, memory_order_relaxed, memory_order_relaxed);
        }
        return retval;
    }
    return 0;
}

int
h2o_nif_port_output_term(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifEnv *msg_env, ERL_NIF_TERM msg)
{
    // TRACE_F("h2o_nif_port_output_term START\n");
    int retval;
    if ((retval = h2o_nif_port_output_send(port, env, msg_env, msg)) != 0) {
        // TRACE_F("h2o_nif_port_output_term SENT\n");
        return retval;
    }
    // TRACE_F("h2o_nif_port_output_term NOT SENT\n");
    h2o_nif_msg_term_t *msg_term = (h2o_nif_msg_term_t *)enif_alloc(sizeof(h2o_nif_msg_term_t));
    if (msg_term == NULL) {
        return 0;
    }
    msg_term->super.type = H2O_NIF_MSG_TERM;
    msg_term->env = enif_alloc_env();
    if (msg_term->env == NULL) {
        (void)enif_free(msg_term);
        return 0;
    }
    msg_term->term = enif_make_copy(msg_term->env, msg);
    retval = h2o_nif_port_output_enqueue(port, (h2o_nif_msg_t *)msg_term);
    if (!retval) {
        // TRACE_F("h2o_nif_port_output_term FAILED TO ENQUEUE\n");
        (void)enif_free_env(msg_term->env);
        (void)enif_free(msg_term);
        return 0;
    }
    // TRACE_F("h2o_nif_port_output_term ENQUEUED\n");
    return retval;
}

int
h2o_nif_port_output_flush(ErlNifEnv *env, h2o_nif_port_t *port)
{
    bool in_nif_call = (env != NULL);
    ck_fifo_mpmc_entry_t *garbage = NULL;
    h2o_nif_msg_t *nif_msg = NULL;
    h2o_nif_msg_term_t *nif_msg_term = NULL;
    ERL_NIF_TERM msg;
    ErlNifEnv *msg_env = NULL;
    ErlNifPid owner;
    int active;
    int retval;
    do {
        owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
        if ((active = h2o_nif_port_output_reserve_send(port)) == 0) {
            break;
        }
        retval = ck_fifo_mpmc_dequeue(&port->output, (void *)&nif_msg, &garbage);
        if (retval) {
            (void)atomic_fetch_sub_explicit(&port->num_output, 1, memory_order_relaxed);
            nif_msg_term = (h2o_nif_msg_term_t *)nif_msg;
            assert(nif_msg_term->super.type == H2O_NIF_MSG_TERM);
            if (in_nif_call) {
                msg = enif_make_tuple3(env, ATOM_h2o_port_data, h2o_nif_port_make(env, port),
                                       enif_make_copy(env, nif_msg_term->term));
                (void)enif_send(env, &owner, NULL, msg);
            } else {
                msg_env = nif_msg_term->env;
                msg = enif_make_tuple3(msg_env, ATOM_h2o_port_data, h2o_nif_port_make(msg_env, port),
                                       enif_make_copy(msg_env, nif_msg_term->term));
                (void)enif_send(NULL, &owner, msg_env, msg);
            }
            (void)enif_free_env(nif_msg_term->env);
            (void)enif_free(nif_msg);
            (void)free(garbage);
            garbage = NULL;
            nif_msg = NULL;
            nif_msg_term = NULL;
            msg_env = NULL;
            msg = (ERL_NIF_TERM)NULL;
        } else if (active == H2O_NIF_PORT_ACTIVE_ONCE) {
            retval = H2O_NIF_PORT_ACTIVE_FALSE;
            (void)atomic_compare_exchange_weak_explicit(&port->active, &retval, active, memory_order_relaxed, memory_order_relaxed);
        }
    } while (retval != 0);
    return 1;
}

typedef struct h2o_nif_port_gc_ctx_s {
    ErlNifEnv *env;
    int num_garbage;
    h2o_nif_port_t *garbage[H2O_NIF_PORT_GC_MAX];
} h2o_nif_port_gc_ctx_t;

// static _Atomic int fake_counter = 0;

static int
h2o_nif_port_gc_iterator(uintptr_t key, uintptr_t value, uintptr_t *acc)
{
    h2o_nif_port_gc_ctx_t *ctx = (h2o_nif_port_gc_ctx_t *)*acc;
    h2o_nif_port_t *port = (h2o_nif_port_t *)value;
    ErlNifPid owner;
    (void)h2o_nif_port_keep(port);
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return 1;
    }
    owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
    if (!enif_is_process_alive(ctx->env, &owner)) {
        // TRACE_F("port %llu is dead (num_garbage = %d)\n", port->id, ctx->num_garbage);
        ctx->garbage[ctx->num_garbage++] = port;
        if (ctx->num_garbage >= H2O_NIF_PORT_GC_MAX) {
            return 0;
        }
    } else {
        // ERL_NIF_TERM msg = ATOM_true;
        // int x = atomic_fetch_add_explicit(&fake_counter, 1, memory_order_relaxed);
        // (void)h2o_nif_port_output_term(port, NULL, ctx->env, enif_make_int(ctx->env, x));
        // (void)enif_clear_env(ctx->env);
        // TRACE_F("port %llu is not dead\n", port->id);
        (void)h2o_nif_port_release(port);
    }
    return 1;
}

static inline void
h2o_nif_port_gc_clear(h2o_nif_port_gc_ctx_t *ctx)
{
    h2o_nif_port_t *port = NULL;
    for (int i = 0; i < ctx->num_garbage; i++) {
        port = ctx->garbage[i];
        (void)h2o_nif_port_close(NULL, port, NULL);
        (void)h2o_nif_port_release(port);
    }
}

static void *
h2o_nif_port_gc_thread(void *arg)
{
    h2o_nif_port_gc_ctx_t _ctx;
    h2o_nif_port_gc_ctx_t *ctx = &_ctx;
    int retval;
    struct timeval start;
    struct timeval stop;
    struct timeval timeslice;
    unsigned long long gc_delta;
    unsigned long long gc_expected;
    unsigned long long gc_desired;

    (void)arg;

    (void)gettimeofday(&start, NULL);

    ctx->env = enif_alloc_env();
    ctx->num_garbage = 0;
    (void)memset(ctx->garbage, 0, sizeof(h2o_nif_port_t *) * H2O_NIF_PORT_GC_MAX);

    do {
        retval = h2o_nif_hm_foreach(h2o_nif_port_gc_iterator, (uintptr_t *)&ctx);
        (void)h2o_nif_port_gc_clear(ctx);
        if (retval == 0) {
            (void)memset(ctx->garbage, 0, sizeof(h2o_nif_port_t *) * ctx->num_garbage);
            ctx->num_garbage = 0;
            (void)ck_pr_stall();
        }
    } while (retval == 0);

    (void)enif_free_env(ctx->env);
    (void)ck_spinlock_unlock(&port_gc_spinlock);

    (void)gettimeofday(&stop, NULL);
    timersub(&stop, &start, &timeslice);
    gc_delta = timeslice.tv_sec * 1000000 + timeslice.tv_usec;
    do {
        gc_expected = atomic_load_explicit(&port_gc_min, memory_order_relaxed);
        if (gc_expected == 0 || gc_delta < gc_expected) {
            gc_desired = gc_delta;
            retval = atomic_compare_exchange_weak_explicit(&port_gc_min, &gc_expected, gc_desired, memory_order_relaxed,
                                                           memory_order_relaxed);
        } else {
            retval = 1;
        }
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    do {
        gc_expected = atomic_load_explicit(&port_gc_max, memory_order_relaxed);
        if (gc_delta > gc_expected) {
            gc_desired = gc_delta;
            retval = atomic_compare_exchange_weak_explicit(&port_gc_max, &gc_expected, gc_desired, memory_order_relaxed,
                                                           memory_order_relaxed);
        } else {
            retval = 1;
        }
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);
    do {
        gc_expected = atomic_load_explicit(&port_gc_avg, memory_order_relaxed);
        gc_desired = (gc_expected + gc_delta) / 2;
        retval = atomic_compare_exchange_weak_explicit(&port_gc_avg, &gc_expected, gc_desired, memory_order_relaxed,
                                                       memory_order_relaxed);
        if (retval == 0) {
            (void)ck_pr_stall();
        }
    } while (retval == 0);

    // {
    //     h2o_nif_port_t *my_port = NULL;
    //     if (h2o_nif_hm_get((uintptr_t)0, (uintptr_t *)&my_port, true)) {
    //         h2o_nif_port_listen_t *port_listen = (h2o_nif_port_listen_t *)enif_alloc(sizeof(h2o_nif_port_listen_t));
    //         (void)memset(port_listen, 0, sizeof(h2o_nif_port_listen_t));
    //         (void)h2o_nif_port_listen_dispatch(my_port, port_listen);
    //         (void)h2o_nif_port_release(my_port);
    //     }
    // }

    return NULL;
}

int
h2o_nif_port_gc_start(void)
{
    if (ck_spinlock_trylock(&port_gc_spinlock)) {
        if (enif_thread_create("h2o_nif_port_gc", &port_gc_tid, h2o_nif_port_gc_thread, NULL, NULL) == 0) {
            return 1;
        } else {
            return 0;
        }
    }
    return 1;
}
