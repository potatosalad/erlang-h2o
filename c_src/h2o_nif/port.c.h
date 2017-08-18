// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../port.h"

/* fun h2o_nif:port_close/1 */

static ERL_NIF_TERM h2o_nif_port_close_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_port_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    ERL_NIF_TERM out;
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port) || h2o_nif_port_is_closed(port)) {
        return ATOM_ok;
    }
    if (!h2o_nif_port_stop(port, env, &out)) {
        return out;
    }
    if (out == ATOM_trap) {
        return enif_schedule_nif(env, "port_close", 0, h2o_nif_port_close_trap_1, argc, argv);
    }
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_close_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    ERL_NIF_TERM out;
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port)) {
        return ATOM_ok;
    }
    if (!h2o_nif_port_stop_continue(port, env, &out)) {
        return out;
    }
    if (out == ATOM_trap) {
        return enif_schedule_nif(env, "port_close", 0, h2o_nif_port_close_trap_1, argc, argv);
    }
    return out;
}

/* fun h2o_nif:port_connect/2 */

static ERL_NIF_TERM
h2o_nif_port_connect_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ErlNifPid new_owner;
    if (!enif_get_local_pid(env, argv[1], &new_owner)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_connect(port, env, new_owner)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    return ATOM_true;
}

/* fun h2o_nif:port_info/1 */

static ERL_NIF_TERM
h2o_nif_port_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM list[5];
    int i = 0;
    /* connected */
    {
        ErlNifPid owner = h2o_nif_port_get_owner(port);
        list[i++] = enif_make_tuple2(env, ATOM_connected, enif_make_pid(env, &owner));
    }
    /* num_children */
    {
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        list[i++] = enif_make_tuple2(env, ATOM_num_children, enif_make_ulong(env, port->num_children));
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
    }
    /* parent */
    {
        ERL_NIF_TERM parent_term;
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        parent_term = (port->parent == NULL) ? ATOM_undefined : h2o_nif_port_make(env, port->parent);
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
        list[i++] = enif_make_tuple2(env, ATOM_parent, parent_term);
    }
    /* state */
    {
        list[i++] = enif_make_tuple2(env, ATOM_state, h2o_nif_port_state_to_atom(port));
    }
    /* type */
    list[i++] = enif_make_tuple2(env, ATOM_type, enif_make_int(env, port->type));
    ERL_NIF_TERM out;
    out = enif_make_list_from_array(env, list, i);
    return out;
}

/* fun h2o_nif:port_info/2 */

static ERL_NIF_TERM
h2o_nif_port_info_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    if (argv[1] == ATOM_children) {
        h2o_linklist_t *anchor = NULL;
        h2o_linklist_t *node = NULL;
        h2o_nif_port_t *child = NULL;
        out = enif_make_list(env, 0);
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        anchor = &port->children;
        node = anchor->prev;
        while (node != anchor) {
            child = (h2o_nif_port_t *)node;
            node = node->prev;
            out = enif_make_list_cell(env, h2o_nif_port_make(env, child), out);
        }
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
    } else if (argv[1] == ATOM_connected) {
        ErlNifPid owner = h2o_nif_port_get_owner(port);
        out = enif_make_pid(env, &owner);
    } else if (argv[1] == ATOM_num_children) {
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        out = enif_make_int(env, enif_make_ulong(env, port->num_children));
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
    } else if (argv[1] == ATOM_parent) {
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        out = (port->parent == NULL) ? ATOM_undefined : h2o_nif_port_make(env, port->parent);
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
    } else if (argv[1] == ATOM_state) {
        out = h2o_nif_port_state_to_atom(port);
    } else if (argv[1] == ATOM_type) {
        out = enif_make_int(env, enif_make_int(env, port->type));
    } else {
        return enif_make_badarg(env);
    }
    out = enif_make_tuple2(env, argv[1], out);
    return out;
}

/* fun h2o_nif:port_is_alive/1 */

static ERL_NIF_TERM
h2o_nif_port_is_alive_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port)) {
        return ATOM_false;
    }
    if (h2o_nif_port_is_closed(port)) {
        return ATOM_false;
    }
    return ATOM_true;
}
