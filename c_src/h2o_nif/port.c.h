// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

static ERL_NIF_TERM
h2o_nif_port_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    h2o_nif_port_t *port = h2o_nif_port_create(env, NULL);
    if (port == NULL) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, port);
    (void)enif_release_resource((void *)port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_open_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rwlock(port)) {
        return enif_make_badarg(env);
    }
    h2o_nif_port_t *child = h2o_nif_port_create(env, port);
    if (child == NULL) {
        (void)h2o_nif_port_rwunlock(port);
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM out = h2o_nif_port_make(env, child);
    (void)h2o_nif_port_rwunlock(port);
    (void)enif_release_resource((void *)child);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // TRACE_F("h2o_nif_port_close_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        return ATOM_ok;
        // return enif_make_badarg(env);
    }
    // TRACE_F("trying to close port %d\n", port->id);
    if (!h2o_nif_port_tryclose(port)) {
        return ATOM_ok;
    }
    // TRACE_F("actually closing port %d\n", port->id);
    (void)h2o_nif_port_close(env, port);
    (void)h2o_nif_port_rwunlock(port);
    if (!h2o_nif_port_tryrelease(port)) {
        return ATOM_ok;
    }
    (void)h2o_nif_port_release(port);
    return ATOM_ok;
    // if (!h2o_nif_port_rwlock(port)) {
    //     return ATOM_ok;
    //     // return enif_make_badarg(env);
    // }
    // if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
    //     (void)h2o_nif_port_rwunlock(port);
    //     return ATOM_ok;
    // }
    // (void)h2o_nif_port_close(env, port);
    // (void)h2o_nif_port_rwunlock(port);
    // return ATOM_ok;
}

static ERL_NIF_TERM
h2o_nif_port_connect_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    ErlNifPid new_owner;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port) || !enif_get_local_pid(env, argv[1], &new_owner)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rwlock(port)) {
        return enif_make_badarg(env);
    }
    if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
        (void)h2o_nif_port_rwunlock(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    port->owner = new_owner;
    (void)h2o_nif_port_rwunlock(port);
    return ATOM_true;
}

static ERL_NIF_TERM
h2o_nif_port_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list[4];
    int i = 0;
    (void)enif_rwlock_rlock(h2o_nif_rwlock);
    /* kh_n_buckets */
    list[i++] = enif_make_tuple2(env, ATOM_kh_n_buckets, enif_make_int(env, kh_n_buckets(h2o_nif_ports)));
    /* kh_size */
    list[i++] = enif_make_tuple2(env, ATOM_kh_size, enif_make_int(env, kh_size(h2o_nif_ports)));
    (void)enif_rwlock_runlock(h2o_nif_rwlock);
    /* mem_info */
    // list[i++] = enif_make_tuple2(env, ATOM_mem_info, h2o_nif_port_mem_info(env));
    /* num_ports */
    list[i++] = enif_make_tuple2(env, ATOM_num_ports, enif_make_int(env, (int)h2o_nif_num_ports));
    /* seq_ports */
    list[i++] = enif_make_tuple2(env, ATOM_seq_ports, enif_make_int(env, (int)h2o_nif_seq_ports));
    ERL_NIF_TERM out;
    out = enif_make_list_from_array(env, list, i);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rlock(port)) {
        return enif_make_badarg(env);
    }
    if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
        (void)h2o_nif_port_runlock(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM list[4];
    int i = 0;
    /* children */
    {
        h2o_linklist_t *anchor = &port->children;
        ERL_NIF_TERM term;
        term = enif_make_list(env, 0);
        if (!h2o_linklist_is_empty(anchor)) {
            h2o_linklist_t *node = anchor;
            h2o_nif_port_t *child = NULL;
            while (node->prev != anchor) {
                node = node->prev;
                child = (h2o_nif_port_t *)node;
                term = enif_make_list_cell(env, h2o_nif_port_make(env, child), term);
            }
        }
        list[i++] = enif_make_tuple2(env, ATOM_children, term);
    }
    /* connected */
    list[i++] = enif_make_tuple2(env, ATOM_connected, enif_make_pid(env, &port->owner));
    /* parent */
    list[i++] = enif_make_tuple2(env, ATOM_parent, (port->parent == NULL) ? ATOM_undefined : h2o_nif_port_make(env, port->parent));
    /* type */
    list[i++] = enif_make_tuple2(env, ATOM_type, enif_make_int(env, port->type));
    ERL_NIF_TERM out;
    out = enif_make_list_from_array(env, list, i);
    (void)h2o_nif_port_runlock(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_info_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port) ||
        (argv[1] != ATOM_children && argv[1] != ATOM_connected && argv[1] != ATOM_parent && argv[1] != ATOM_type)) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rlock(port)) {
        return enif_make_badarg(env);
    }
    if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
        (void)h2o_nif_port_runlock(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM out;
    if (argv[1] == ATOM_children) {
        h2o_linklist_t *anchor = &port->children;
        out = enif_make_list(env, 0);
        if (!h2o_linklist_is_empty(anchor)) {
            h2o_linklist_t *node = anchor;
            h2o_nif_port_t *child = NULL;
            while (node->prev != anchor) {
                node = node->prev;
                child = (h2o_nif_port_t *)node;
                out = enif_make_list_cell(env, h2o_nif_port_make(env, child), out);
            }
        }
    } else if (argv[1] == ATOM_connected) {
        out = enif_make_pid(env, &port->owner);
    } else if (argv[1] == ATOM_parent) {
        out = (port->parent == NULL) ? ATOM_undefined : h2o_nif_port_make(env, port->parent);
    } else {
        out = enif_make_int(env, port->type);
    }
    out = enif_make_tuple2(env, argv[1], out);
    (void)h2o_nif_port_runlock(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_getopt_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    h2o_nif_port_t *port = NULL;
    if (!h2o_nif_port_get(env, argv[0], &port) || argv[1] != ATOM_active) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rlock(port)) {
        return enif_make_badarg(env);
    }
    if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
        (void)h2o_nif_port_runlock(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM out;
    switch (port->active) {
    case H2O_NIF_PORT_ACTIVE_FALSE:
        out = ATOM_false;
        break;
    case H2O_NIF_PORT_ACTIVE_TRUE:
        out = ATOM_true;
        break;
    case H2O_NIF_PORT_ACTIVE_ONCE:
        out = ATOM_once;
        break;
    case H2O_NIF_PORT_ACTIVE_MULTI:
        out = enif_make_int(env, port->active_count);
        break;
    }
    out = enif_make_tuple2(env, ATOM_ok, out);
    (void)h2o_nif_port_runlock(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_setopt_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 3) {
        return enif_make_badarg(env);
    }
    h2o_nif_port_t *port = NULL;
    int active_count = 0;
    if (!h2o_nif_port_get(env, argv[0], &port) || argv[1] != ATOM_active ||
        (argv[2] != ATOM_false && argv[2] != ATOM_once && argv[2] != ATOM_true && !enif_get_int(env, argv[2], &active_count))) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_rwlock(port)) {
        return enif_make_badarg(env);
    }
    if (port->state == H2O_NIF_PORT_STATE_CLOSED) {
        (void)h2o_nif_port_rwunlock(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (argv[2] == ATOM_false) {
        port->active = H2O_NIF_PORT_ACTIVE_FALSE;
        port->active_count = 0;
    } else if (argv[2] == ATOM_once) {
        port->active = H2O_NIF_PORT_ACTIVE_ONCE;
        port->active_count = 0;
    } else if (argv[2] == ATOM_true) {
        port->active = H2O_NIF_PORT_ACTIVE_TRUE;
        port->active_count = 0;
    } else {
        port->active = H2O_NIF_PORT_ACTIVE_MULTI;
        port->active_count += active_count;
    }
    (void)h2o_nif_port_rwunlock(port);
    return ATOM_ok;
}
