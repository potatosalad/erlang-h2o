// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "port.h"

/* Global Variables */

ck_spinlock_t h2o_nif_ports_spinlock = CK_SPINLOCK_INITIALIZER;
ErlNifResourceType *h2o_nif_port_resource_type = NULL;

/* NIF Functions */

static h2o_nif_port_t *h2o_nif_port_alloc(size_t size);
static void h2o_nif_port_dtor(ErlNifEnv *env, void *obj);
static void h2o_nif_port_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon);

static ErlNifResourceTypeInit h2o_nif_port_resource_type_init = {
    .dtor = h2o_nif_port_dtor, .stop = NULL, .down = h2o_nif_port_down};

int
h2o_nif_port_load(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    h2o_nif_port_resource_type = enif_open_resource_type_x(env, "h2o_nif_port", &h2o_nif_port_resource_type_init,
                                                           ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return 0;
}

int
h2o_nif_port_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

void
h2o_nif_port_unload(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    return;
}

/* Resource Functions */

static h2o_nif_port_t *
h2o_nif_port_alloc(size_t size)
{
    TRACE_F("h2o_nif_port_alloc:%s:%d\n", __FILE__, __LINE__);
    assert(size >= sizeof(h2o_nif_port_t));
    h2o_nif_port_t *port = (void *)enif_alloc_resource(h2o_nif_port_resource_type, size);
    if (port == NULL) {
        return NULL;
    }
    (void)memset(port, 0, size);
    atomic_init(&port->state, ((h2o_nif_port_state_t){H2O_NIF_PORT_STATE_ALLOCATED, 0}));
    (void)ck_spinlock_init(&port->owner.lock);
    (void)h2o_linklist_init_anchor(&port->children);
    (void)h2o_linklist_init_anchor(&port->on_close.children);
    // atomic_init(&port->num_children, 0);
    return port;
}

static void
h2o_nif_port_dtor(ErlNifEnv *env, void *obj)
{
    TRACE_F("h2o_nif_port_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = (h2o_nif_port_t *)obj;
    if (!h2o_nif_port_is_closed(port)) {
        TRACE_F("SHOULD NEVER HAPPEN: port dtor called for non-closed port\n");
        perror("non-closed port dtor:");
        exit(1);
    }
    if (port->dtor != NULL) {
        (void)port->dtor(env, port);
    }
    return;
}

static void
h2o_nif_port_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    TRACE_F("h2o_nif_port_down:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = (h2o_nif_port_t *)obj;
    (void)ck_spinlock_lock_eb(&port->owner.lock);
    /* owner process exiting */
    if (enif_compare_monitors(mon, port->owner.mon) == 0) {
        port->owner.mon = NULL;
        (void)ck_spinlock_unlock(&port->owner.lock);
        (void)h2o_nif_port_stop_quiet(port, NULL, NULL);
        return;
    }
    /* stale notification, just ignore */
    (void)ck_spinlock_unlock(&port->owner.lock);
}

/* Port Functions */

static int h2o_nif_port_close(h2o_nif_port_t *port);

int
h2o_nif_port_open(h2o_nif_port_t *parent, h2o_nif_port_init_t *port_init, size_t size, h2o_nif_port_t **portp)
{
    TRACE_F("h2o_nif_port_open:%s:%d\n", __FILE__, __LINE__);
    assert(portp != NULL);
    assert(port_init != NULL);
    h2o_nif_port_t *port = h2o_nif_port_alloc(size);
    if (port == NULL) {
        *portp = NULL;
        return 0;
    }
    if (parent != NULL) {
        (void)h2o_nif_port_keep(parent);
        port->parent = parent;
        (void)h2o_nif_port_keep(port);
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        (void)h2o_linklist_insert(&parent->children, &port->_link);
        parent->num_children++;
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        // (void)atomic_fetch_add_explicit(&parent->num_children, 1, memory_order_relaxed);
        port->owner.pid = h2o_nif_port_get_owner(parent);
        // (void)h2o_nif_port_set_owner(port, h2o_nif_port_get_owner(parent));
    }
    (void)ck_spinlock_init(&port->_sentinel_lock);
    port->_sentinel.refc = 0;
    port->_sentinel.ptr = NULL;
    port->type = port_init->type;
    port->stop = port_init->stop;
    port->dtor = port_init->dtor;
    *portp = port;
    return 1;
}

int
h2o_nif_port_connect(h2o_nif_port_t *port, ErlNifEnv *env, ErlNifPid new_owner)
{
    TRACE_F("h2o_nif_port_connect:%s:%d\n", __FILE__, __LINE__);
    ErlNifMonitor *old_mon = NULL;
    ErlNifMonitor *new_mon = NULL;
    (void)ck_spinlock_lock_eb(&port->owner.lock);
    old_mon = port->owner.mon;
    new_mon = (old_mon == &port->owner.mons[0]) ? &port->owner.mons[1] : &port->owner.mons[0];
    if (enif_monitor_process(env, (void *)port, &new_owner, new_mon) != 0) {
        (void)ck_spinlock_unlock(&port->owner.lock);
        return 0;
    }
    port->owner.mon = new_mon;
    port->owner.pid = new_owner;
    if (old_mon != NULL) {
        (void)enif_demonitor_process(env, (void *)port, old_mon);
    }
    (void)ck_spinlock_unlock(&port->owner.lock);
    return 1;
    // (void)h2o_nif_port_set_owner(port, new_owner);
    // if (atomic_load_explicit(&port->state, memory_order_relaxed) == H2O_NIF_PORT_STATE_CLOSED) {
    //     if (((port->on_close.state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) && !port->on_close.silent) {
    //         ErlNifEnv *msg_env = (env != NULL) ? env : enif_alloc_env();
    //         ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
    //         if (env != NULL) {
    //             (void)h2o_nif_port_send(msg_env, port, NULL, msg);
    //         } else {
    //             (void)h2o_nif_port_send(NULL, port, msg_env, msg);
    //             (void)enif_free_env(msg_env);
    //         }
    //     }
    // }
}

static void on_port_sentinel_dtor(void *data);

int
h2o_nif_port_sentinel_link(h2o_mem_pool_t *pool, h2o_nif_port_t *port, h2o_nif_port_sentinel_dtor_t *dtor)
{
    TRACE_F("h2o_nif_port_sentinel_link:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_sentinel_t *sentinel = NULL;
    (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
    if (port->_sentinel.refc != 0 || port->_sentinel.ptr != NULL) {
        (void)ck_spinlock_unlock(&port->_sentinel_lock);
        return 0;
    }
    sentinel = (void *)h2o_mem_alloc_shared(pool, sizeof(h2o_nif_port_sentinel_t), on_port_sentinel_dtor);
    (void)h2o_nif_port_keep(port);
    sentinel->port = port;
    sentinel->dtor = dtor;
    port->_sentinel.refc = 3;
    port->_sentinel.ptr = sentinel;
    (void)ck_spinlock_unlock(&port->_sentinel_lock);
    return 1;
}

static void
on_port_sentinel_dtor(void *data)
{
    TRACE_F("on_port_sentinel_dtor:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_sentinel_t *sentinel = data;
    if (sentinel->port == NULL && sentinel->dtor == NULL) {
        return;
    }
    h2o_nif_port_t *port = sentinel->port;
    h2o_nif_port_sentinel_ref_t expected;
    h2o_nif_port_sentinel_ref_t desired;
    do {
        (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
        expected = port->_sentinel;
        (void)ck_spinlock_unlock(&port->_sentinel_lock);
        switch (expected.refc) {
        case 0:
            (void)h2o_nif_port_release(port);
            return;
        case 1: /* evloop is done, scheduler will run dtor */
            (void)h2o_nif_port_sentinel_unlink(port);
            (void)h2o_nif_port_release(port);
            return;
        case 2: /* scheduler is done, evloop will run dtor */
            desired.refc = 0;
            desired.ptr = NULL;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                assert(expected.ptr != NULL);
                (void)expected.ptr->dtor(port);
                (void)h2o_nif_port_release(port);
                return;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        case 3: /* evloop is done, scheduler will run dtor */
            desired.refc = 1;
            desired.ptr = expected.ptr;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                assert(expected.ptr != NULL);
                (void)h2o_nif_port_sentinel_unlink(port);
                (void)h2o_nif_port_release(port);
                return;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        default:
            break;
        }
        (void)ck_pr_stall();
    } while (1);
}

void
h2o_nif_port_sentinel_unlink(h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_port_sentinel_unlink:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_sentinel_ref_t expected;
    h2o_nif_port_sentinel_ref_t desired;
    do {
        (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
        expected = port->_sentinel;
        (void)ck_spinlock_unlock(&port->_sentinel_lock);
        switch (expected.refc) {
        case 0:
            return;
        case 1: /* evloop is done, scheduler will run dtor */
            desired.refc = 0;
            desired.ptr = NULL;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                assert(expected.ptr != NULL);
                (void)expected.ptr->dtor(port);
                return;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        case 2: /* scheduler is done, allow evloop to finish in another thread */
            break;
        case 3: /* scheduler is done, evloop will run dtor */
            desired.refc = 2;
            desired.ptr = expected.ptr;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                assert(expected.ptr != NULL);
                (void)h2o_mem_release_shared(expected.ptr);
                return;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        default:
            break;
        }
        (void)ck_pr_stall();
    } while (1);
}

int
h2o_nif_port_sentinel_trylock(h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_port_sentinel_trylock:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_sentinel_ref_t expected;
    h2o_nif_port_sentinel_ref_t desired;
    do {
        (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
        expected = port->_sentinel;
        (void)ck_spinlock_unlock(&port->_sentinel_lock);
        switch (expected.refc) {
        case 0:
        case 1:
        case 2:
            return 0;
        default:
            desired.refc = expected.refc + 1;
            desired.ptr = expected.ptr;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                return 1;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        }
        (void)ck_pr_stall();
    } while (1);
}

int
h2o_nif_port_sentinel_tryunlock(h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_port_sentinel_tryunlock:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_sentinel_t *sentinel = NULL;
    h2o_nif_port_sentinel_ref_t expected;
    h2o_nif_port_sentinel_ref_t desired;
    do {
        (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
        expected = port->_sentinel;
        (void)ck_spinlock_unlock(&port->_sentinel_lock);
        switch (expected.refc) {
        case 0:
        case 1:
        case 2:
        case 3:
            return 0;
        default:
            desired.refc = expected.refc - 1;
            desired.ptr = expected.ptr;
            (void)ck_spinlock_lock_eb(&port->_sentinel_lock);
            if (memcmp(&port->_sentinel, &expected, sizeof(h2o_nif_port_sentinel_ref_t)) == 0) {
                port->_sentinel = desired;
                (void)ck_spinlock_unlock(&port->_sentinel_lock);
                return 1;
            }
            (void)ck_spinlock_unlock(&port->_sentinel_lock);
            break;
        }
        (void)ck_pr_stall();
    } while (1);
}

static int
h2o_nif_port_close(h2o_nif_port_t *port)
{
    TRACE_F("h2o_nif_port_close:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_state_t expected;
    h2o_nif_port_state_t desired;
    int fetched;
    int result;
    desired = (h2o_nif_port_state_t){H2O_NIF_PORT_STATE_CLOSED, 0};
    do {
        expected = atomic_load_explicit(&port->state, memory_order_relaxed);
        fetched = expected.flags;
        if (fetched == H2O_NIF_PORT_STATE_CLOSED) {
            return fetched;
        }
        expected.async = 0;
        result =
            atomic_compare_exchange_weak_explicit(&port->state, &expected, desired, memory_order_relaxed, memory_order_relaxed);
        if (!result) {
            (void)ck_pr_stall();
        }
    } while (!result);
    return fetched;
}

int
h2o_nif_port_stop(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    TRACE_F("h2o_nif_port_stop:%s:%d\n", __FILE__, __LINE__);
    int state = h2o_nif_port_close(port);
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        /* Already closed by another thread */
        if (out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    port->on_close.state = state;
    return h2o_nif_port_stop_continue(port, env, out);
}

int
h2o_nif_port_stop_quiet(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    TRACE_F("h2o_nif_port_stop_quiet:%s:%d\n", __FILE__, __LINE__);
    int state = h2o_nif_port_close(port);
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        /* Already closed by another thread */
        if (out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    port->on_close.state = state;
    port->on_close.quiet = 1;
    return h2o_nif_port_stop_continue(port, env, out);
}

static ERL_NIF_TERM
h2o_nif_port_stop_immediate(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    TRACE_F("h2o_nif_port_stop_immediate:%s:%d\n", __FILE__, __LINE__);
    ERL_NIF_TERM result_term = ATOM_ok;
    /* This function is unsafe as-is, but here's an extra safety check */
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        return result_term;
    }
    /* Run stop callback if present */
    if (port->stop != NULL) {
        result_term = port->stop(env, port, is_direct_call);
        while (result_term == ATOM_trap) {
            (void)ck_pr_stall();
            result_term = port->stop(env, port, is_direct_call);
        }
    }
    return result_term;
}

int
h2o_nif_port_stop_continue(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    TRACE_F("h2o_nif_port_stop_continue:%s:%d\n", __FILE__, __LINE__);
    int is_direct_call = (env == NULL) ? 0 : 1;
    ERL_NIF_TERM result_term = ATOM_ok;
    h2o_linklist_t *child_link = NULL;
    /* This function is unsafe as-is, but here's an extra safety check */
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        if (is_direct_call && out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    /* Run stop callback if present */
    if (port->stop != NULL) {
        result_term = port->stop(env, port, is_direct_call);
        if (result_term == ATOM_trap) {
            if (is_direct_call && out != NULL) {
                *out = result_term;
                return 1;
            }
            (void)h2o_nif_port_stop_immediate(env, port, is_direct_call);
        }
        /* Finished with stop callback */
        port->stop = NULL;
    }
    /* Run stop callback for child if present */
    child_link = port->on_close.child;
    if (child_link != NULL) {
        h2o_nif_port_t *child = H2O_STRUCT_FROM_MEMBER(h2o_nif_port_t, _link, child_link);
        (void)h2o_nif_port_stop_continue(child, env, &result_term);
        if (result_term == ATOM_trap) {
            if (is_direct_call && out != NULL) {
                *out = result_term;
                return 1;
            }
            (void)h2o_nif_port_stop_immediate(env, child, is_direct_call);
        }
        /* Finished with stop callback for child */
        port->on_close.child = NULL;
    }
    /* Send closed message to owner if port was open */
    if (((port->on_close.state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) && !port->on_close.quiet) {
        ErlNifEnv *msg_env = (is_direct_call) ? env : enif_alloc_env();
        ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
        if (is_direct_call) {
            (void)h2o_nif_port_send(msg_env, port, NULL, msg);
        } else {
            (void)h2o_nif_port_send(NULL, port, msg_env, msg);
            (void)enif_free_env(msg_env);
        }
        port->on_close.quiet = 1;
    }
    /* Unlink from parent, if present */
    {
        h2o_nif_port_t *parent = NULL;
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        parent = port->parent;
        if (parent != NULL) {
            (void)h2o_linklist_unlink(&port->_link);
            port->parent = NULL;
            parent->num_children--;
            if (parent->on_close.child == &port->_link) {
                parent->on_close.child = NULL;
            }
            (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
            (void)h2o_nif_port_release(parent);
            (void)h2o_nif_port_release(port);
        } else {
            (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        }
    }
    /* Stop all children, if present */
    {
        h2o_linklist_t *anchor = NULL;
        h2o_linklist_t *node = NULL;
        h2o_nif_port_t *child = NULL;
        h2o_linklist_t garbage;
        size_t num_garbage = 0;
        (void)h2o_linklist_init_anchor(&garbage);
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        (void)h2o_linklist_insert_list(&garbage, &port->on_close.children);
        (void)h2o_linklist_insert_list(&garbage, &port->children);
        port->num_children = 0;
        port->on_close.num_children = 0;
        anchor = &garbage;
        node = anchor->next;
        while (node != anchor) {
            child = H2O_STRUCT_FROM_MEMBER(h2o_nif_port_t, _link, node);
            node = node->next;
            if (!h2o_nif_port_is_closed(child)) {
                child->parent = NULL;
                num_garbage++;
            } else {
                /* Child already closing in progress by another thread, allow it to unlink itself */
                (void)h2o_linklist_unlink(&child->_link);
                (void)h2o_linklist_insert(&port->children, &child->_link);
                port->num_children++;
            }
        }
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        anchor = &garbage;
        node = anchor->next;
        while (node != anchor) {
            child = H2O_STRUCT_FROM_MEMBER(h2o_nif_port_t, _link, node);
            node = node->next;
            (void)h2o_nif_port_stop(child, env, &result_term);
            if (result_term == ATOM_trap) {
                if (is_direct_call && out != NULL) {
                    (void)h2o_linklist_insert_list(&port->on_close.children, &garbage);
                    port->on_close.num_children = num_garbage;
                    port->on_close.child = &child->_link;
                    *out = result_term;
                    return 1;
                }
                (void)h2o_nif_port_stop_immediate(env, child, is_direct_call);
            }
            num_garbage--;
            (void)h2o_nif_port_release(child);
            (void)h2o_nif_port_release(port);
        }
    }
    /* Final release (cancels out allocation) */
    (void)h2o_nif_port_release(port);
    /* Set the out term, if present */
    if (is_direct_call && out != NULL) {
        *out = result_term;
    }
    return 1;
}
