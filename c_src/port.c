// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "port.h"

ck_spinlock_t h2o_nif_ports_spinlock = CK_SPINLOCK_INITIALIZER;
ErlNifResourceType *h2o_nif_port_resource_type = NULL;

/* NIF Functions */

int
h2o_nif_port_load(ErlNifEnv *env, h2o_nif_data_t *nif_data)
{
    h2o_nif_port_resource_type =
        enif_open_resource_type(env, NULL, "h2o_nif_port", h2o_nif_port_dtor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
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

h2o_nif_port_t *
h2o_nif_port_alloc(size_t size)
{
    assert(size >= sizeof(h2o_nif_port_t));
    h2o_nif_port_t *port = (h2o_nif_port_t *)enif_alloc_resource(h2o_nif_port_resource_type, size);
    if (port == NULL) {
        return NULL;
    }
    (void)memset(port, 0, size);
    (void)atomic_init(&port->state, H2O_NIF_PORT_STATE_ALLOCATED);
    (void)h2o_linklist_init_anchor(&port->children);
    (void)atomic_init(&port->num_children, 0);
    return port;
}

void
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

/* Port Functions */

int
h2o_nif_port_open(h2o_nif_port_t *parent, size_t size, h2o_nif_port_t **portp)
{
    assert(portp != NULL);
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
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        (void)atomic_fetch_add_explicit(&parent->num_children, 1, memory_order_relaxed);
        (void)h2o_nif_port_set_owner(port, h2o_nif_port_get_owner(parent));
    }
    *portp = port;
    return 1;
}

int
h2o_nif_port_close(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    int state = __h2o_nif_port_fetch_and_swap_state(port, H2O_NIF_PORT_STATE_CLOSED);
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        // Already closed by another thread
        if (out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    port->on_close.state = state;
    return (__h2o_nif_port_close(port, env, out));
}

int
h2o_nif_port_close_silent(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    int state = __h2o_nif_port_fetch_and_swap_state(port, H2O_NIF_PORT_STATE_CLOSED);
    if (state == H2O_NIF_PORT_STATE_CLOSED) {
        // Already closed by another thread
        if (out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    port->on_close.state = state;
    port->on_close.silent = 1;
    return (__h2o_nif_port_close(port, env, out));
}

static ERL_NIF_TERM
__h2o_nif_port_close_immediate(ErlNifEnv *env, h2o_nif_port_t *port, int is_direct_call)
{
    ERL_NIF_TERM result_term = ATOM_ok;
    // This function is unsafe as-is, but here's an extra safety check
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        return result_term;
    }
    // Run close callback if present
    if (port->on_close.callback != NULL) {
        result_term = port->on_close.callback(env, port, is_direct_call);
        while (result_term == ATOM_trap) {
            (void)ck_pr_stall();
            result_term = port->on_close.callback(env, port, is_direct_call);
        }
    }
    return result_term;
}

int
__h2o_nif_port_close(h2o_nif_port_t *port, ErlNifEnv *env, ERL_NIF_TERM *out)
{
    int is_direct_call = (env == NULL) ? 0 : 1;
    ERL_NIF_TERM result_term = ATOM_ok;
    // This function is unsafe as-is, but here's an extra safety check
    if (port->on_close.state == H2O_NIF_PORT_STATE_CLOSED) {
        if (is_direct_call && out != NULL) {
            *out = ATOM_ok;
        }
        return 0;
    }
    // Run close callback if present
    if (port->on_close.callback != NULL) {
        result_term = port->on_close.callback(env, port, is_direct_call);
        if (result_term == ATOM_trap) {
            if (is_direct_call && out != NULL) {
                *out = result_term;
                return 1;
            }
            (void)__h2o_nif_port_close_immediate(env, port, is_direct_call);
        }
        // Finished with callback
        port->on_close.callback = NULL;
    }
    // Send closed message to owner if port was open
    if (((port->on_close.state & H2O_NIF_PORT_STATE_OPEN) == H2O_NIF_PORT_STATE_OPEN) && !port->on_close.silent) {
        ErlNifEnv *msg_env = (is_direct_call) ? env : enif_alloc_env();
        ERL_NIF_TERM msg = enif_make_tuple2(msg_env, ATOM_h2o_port_closed, h2o_nif_port_make(msg_env, port));
        if (is_direct_call) {
            (void)h2o_nif_port_send(msg_env, port, NULL, msg);
        } else {
            (void)h2o_nif_port_send(NULL, port, msg_env, msg);
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
            (void)h2o_nif_port_keep(child); // Defer release until later
            if (!h2o_nif_port_is_closed(child)) {
                (void)h2o_linklist_unlink(&child->_link);
                child->parent = NULL;
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
            (void)h2o_nif_port_close(child, env, NULL);
            (void)h2o_nif_port_release(child); // Deferred release from above
            (void)h2o_nif_port_release(child);
            (void)h2o_nif_port_release(port);
        }
    }
    // Final release (cancels out allocation)
    (void)h2o_nif_port_release(port);
    // Set the out term, if present
    if (is_direct_call && out != NULL) {
        *out = result_term;
    }
    return 1;
}
