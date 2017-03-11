// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

// static void *
// fake_kill(void *arg)
// {
//     h2o_nif_port_t *port = (h2o_nif_port_t *)arg;
//     sleep(10);
//     (void)h2o_nif_port_close(NULL, port, NULL);
//     (void)h2o_nif_port_release(port);
//     return NULL;
// }

// static ERL_NIF_TERM
// h2o_nif_port_kill_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
// {
//     h2o_nif_port_t *port = NULL;
//     if (argc != 1) {
//         return enif_make_badarg(env);
//     }
//     if (!h2o_nif_port_get(env, argv[0], &port)) {
//         return ATOM_ok;
//     }
//     if (h2o_nif_port_is_closed(port)) {
//         (void)h2o_nif_port_release(port);
//         return ATOM_ok;
//     }
//     ErlNifTid tid;
//     (void)enif_thread_create("fake_kill", &tid, fake_kill, (void *)port, NULL);
//     return ATOM_ok;
// }

static ERL_NIF_TERM
h2o_nif_port_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 0 || !h2o_nif_port_open(NULL, &port)) {
        return enif_make_badarg(env);
    }
    ErlNifPid owner;
    (void)enif_self(env, &owner);
    (void)h2o_nif_port_set_owner(port, owner);
    (void)h2o_nif_port_cas_set_state(port, H2O_NIF_PORT_STATE_OPEN);
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_open_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *parent = NULL;
    h2o_nif_port_t *child = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &parent)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(parent)) {
        // Parent already closed
        (void)h2o_nif_port_release(parent);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!h2o_nif_port_open(parent, &child)) {
        (void)h2o_nif_port_release(parent);
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(parent)) {
        // Parent has been closed by another thread while opening child
        (void)h2o_nif_port_close(env, child, NULL);
        (void)h2o_nif_port_release(parent);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ErlNifPid owner;
    (void)enif_self(env, &owner);
    (void)h2o_nif_port_set_owner(child, owner);
    (void)h2o_nif_port_cas_set_state(child, H2O_NIF_PORT_STATE_OPEN);
    ERL_NIF_TERM out;
    out = h2o_nif_port_make(env, child);
    (void)h2o_nif_port_release(parent);
    return out;
}

static ERL_NIF_TERM h2o_nif_port_close_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM
h2o_nif_port_close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port)) {
        return ATOM_ok;
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return ATOM_ok;
    }
    H2O_NIF_RETURN on_close;
    if (h2o_nif_port_close(env, port, &on_close) && on_close == H2O_NIF_RETURN_TRAP) {
        ERL_NIF_TERM newargv[1];
        newargv[0] = enif_make_resource(env, (void *)port);
        // Defer resource release until finished
        (void)h2o_nif_port_release(port);
        return enif_schedule_nif(env, "port_close", 0, h2o_nif_port_close_trap_1, 1, newargv);
    }
    ERL_NIF_TERM out = port->on_close.out;
    (void)h2o_nif_port_release(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_close_trap_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    TRACE_F("h2o_nif_port_close_trap_1\n");
    h2o_nif_data_t *priv_data = (h2o_nif_data_t *)(enif_priv_data(env));
    ErlNifResourceType *port_type = priv_data->port;
    h2o_nif_port_t *port = NULL;
    H2O_NIF_RETURN on_close;
    if (argc != 1 || !enif_get_resource(env, argv[0], port_type, (void **)&port)) {
        return enif_make_badarg(env);
    }
    if (_h2o_nif_port_close(env, port, &on_close) && on_close == H2O_NIF_RETURN_TRAP) {
        return enif_schedule_nif(env, "port_close", 0, h2o_nif_port_close_trap_1, argc, argv);
    }
    ERL_NIF_TERM out = port->on_close.out;
    (void)enif_release_resource((void *)port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_connect_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 2 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ErlNifPid new_owner;
    if (!enif_get_local_pid(env, argv[1], &new_owner)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_set_owner(port, new_owner);
    (void)h2o_nif_port_release(port);
    return ATOM_true;
}

static ERL_NIF_TERM
h2o_nif_port_info_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM list[6];
    int i = 0;
    /* hm_stat */
    {
        ERL_NIF_TERM sublist[2];
        int si = 0;
        h2o_nif_hm_stat_t hm_stat = {0, 0};
        if (!h2o_nif_hm_stat(&hm_stat)) {
            TRACE_F("unable to get hm_stat\n");
        }
        sublist[si++] = enif_make_tuple2(env, ATOM_n_buckets, enif_make_uint(env, hm_stat.n_buckets));
        sublist[si++] = enif_make_tuple2(env, ATOM_size, enif_make_uint(env, hm_stat.size));
        list[i++] = enif_make_tuple2(env, ATOM_hm_stat, enif_make_list_from_array(env, sublist, si));
    }
    /* ports_stat */
    {
        ERL_NIF_TERM sublist[4];
        int si = 0;
        h2o_nif_ports_stat_t ports_stat = {0, 0, 0, 0};
        if (!h2o_nif_ports_stat(&ports_stat)) {
            TRACE_F("unable to get ports_stat\n");
        }
        sublist[si++] = enif_make_tuple2(env, ATOM_gc_avg, enif_make_uint(env, ports_stat.gc_avg));
        sublist[si++] = enif_make_tuple2(env, ATOM_gc_max, enif_make_uint(env, ports_stat.gc_max));
        sublist[si++] = enif_make_tuple2(env, ATOM_gc_min, enif_make_uint(env, ports_stat.gc_min));
        sublist[si++] = enif_make_tuple2(env, ATOM_seq, enif_make_uint(env, ports_stat.seq));
        list[i++] = enif_make_tuple2(env, ATOM_ports_stat, enif_make_list_from_array(env, sublist, si));
    }
    // (void)enif_rwlock_rlock(h2o_nif_rwlock);
    // /* kh_n_buckets */
    // list[i++] = enif_make_tuple2(env, ATOM_kh_n_buckets, enif_make_int(env, kh_n_buckets(h2o_nif_ports)));
    // /* kh_size */
    // list[i++] = enif_make_tuple2(env, ATOM_kh_size, enif_make_int(env, kh_size(h2o_nif_ports)));
    // (void)enif_rwlock_runlock(h2o_nif_rwlock);
    /* mem_info */
    // list[i++] = enif_make_tuple2(env, ATOM_mem_info, h2o_nif_port_mem_info(env));
    /* num_ports */
    // list[i++] = enif_make_tuple2(env, ATOM_num_ports, enif_make_int(env, (int)h2o_nif_num_ports));
    /* seq_ports */
    // list[i++] = enif_make_tuple2(env, ATOM_seq_ports, enif_make_int(env, (int)h2o_nif_seq_ports));
    // if (h2o_nif_ht_size() < 20 && y == 0) {
    //     TRACE_F("h2o_nif_ht_put = %d\n", h2o_nif_ht_put((uintptr_t)x++, (uintptr_t)h2o_nif_mutex));
    //     (void)h2o_nif_ht_epoch_barrier();
    // } else if (h2o_nif_ht_size() >= 20 && y == 0) {
    //     TRACE_F("switch to del\n");
    //     y = 1;
    // } else if (h2o_nif_ht_size() > 10 && y == 1) {
    //     TRACE_F("h2o_nif_ht_del = %d\n", h2o_nif_ht_del((uintptr_t)x--));
    //     (void)h2o_nif_ht_epoch_barrier();
    // } else if (h2o_nif_ht_size() <= 10 && y == 1) {
    //     TRACE_F("switch to put\n");
    //     y = 0;
    // }
    // {
    //     h2o_nif_fifo_entry_t *entry = enif_alloc(sizeof(*entry));
    //     (void)memset(entry, 0, sizeof(*entry));
    //     ck_fifo_mpmc_entry_t *fifo_entry = malloc(sizeof(*fifo_entry));
    //     (void)memset(fifo_entry, 0, sizeof(*fifo_entry));
    //     (void)enif_self(env, &entry->pid);
    //     TRACE_F("enqueue\n");
    //     while (ck_fifo_mpmc_tryenqueue(&h2o_nif_fifo, fifo_entry, entry) == false) {
    //         TRACE_F("stalling enqueue\n");
    //         ck_pr_stall();
    //     }
    //     // (void)ck_fifo_mpmc_enqueue(&h2o_nif_fifo, fifo_entry, entry);
    //     TRACE_F("why isn't this working?\n");
    // }
    // list[i++] = enif_make_tuple2(env, enif_make_atom(env, "ht_size"), enif_make_uint(env, h2o_nif_ht_size()));
    // {
    //     ERL_NIF_TERM keys;
    //     ck_ht_iterator_t iterator;
    //     ck_ht_entry_t *entry = NULL;
    //     keys = enif_make_list(env, 0);
    //     (void)h2o_nif_ht_iterator_init(&iterator);
    //     (void)h2o_nif_ht_lock();
    //     while (h2o_nif_ht_next(&iterator, &entry)) {
    //         keys = enif_make_list_cell(env, enif_make_int(env, (int)ck_ht_entry_key_direct(entry)), keys);
    //     }
    //     // if (entry != NULL) {
    //     //     keys = enif_make_list_cell(env, enif_make_int(env, (int)ck_ht_entry_key_direct(entry)), keys);
    //     // }
    //     (void)h2o_nif_ht_unlock();
    //     list[i++] = enif_make_tuple2(env, enif_make_atom(env, "ht_keys"), keys);
    // }
    ERL_NIF_TERM out;
    out = enif_make_list_from_array(env, list, i);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_info_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // TRACE_F("h2o_nif_port_info_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        // TRACE_F("unable to get port\n");
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        // TRACE_F("port is closed? %d\n", atomic_load_explicit(&port->state, memory_order_relaxed));
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM list[8];
    int i = 0;
    /* connected */
    {
        ErlNifPid owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_connected, enif_make_pid(env, &owner));
    }
    /* num_accept */
    {
        int num_accept = atomic_load_explicit(&port->num_accept, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_num_accept, enif_make_int(env, num_accept));
    }
    /* num_children */
    {
        int num_children = atomic_load_explicit(&port->num_children, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_num_children, enif_make_int(env, num_children));
    }
    /* num_listen */
    {
        int num_listen = atomic_load_explicit(&port->num_listen, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_num_listen, enif_make_int(env, num_listen));
    }
    /* num_output */
    {
        int num_output = atomic_load_explicit(&port->num_output, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_num_output, enif_make_int(env, num_output));
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
        int state = atomic_load_explicit(&port->state, memory_order_relaxed);
        list[i++] = enif_make_tuple2(env, ATOM_state, enif_make_int(env, state));
    }
    /* type */
    list[i++] = enif_make_tuple2(env, ATOM_type, enif_make_int(env, port->type));
    ERL_NIF_TERM out;
    out = enif_make_list_from_array(env, list, i);
    (void)h2o_nif_port_release(port);
    return out;
}

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
        ErlNifPid owner = atomic_load_explicit(&port->owner, memory_order_relaxed);
        out = enif_make_pid(env, &owner);
    } else if (argv[1] == ATOM_num_accept) {
        int num_accept = atomic_load_explicit(&port->num_accept, memory_order_relaxed);
        out = enif_make_int(env, num_accept);
    } else if (argv[1] == ATOM_num_children) {
        int num_children = atomic_load_explicit(&port->num_children, memory_order_relaxed);
        out = enif_make_int(env, num_children);
    } else if (argv[1] == ATOM_num_listen) {
        int num_listen = atomic_load_explicit(&port->num_listen, memory_order_relaxed);
        out = enif_make_int(env, num_listen);
    } else if (argv[1] == ATOM_num_output) {
        int num_output = atomic_load_explicit(&port->num_output, memory_order_relaxed);
        out = enif_make_int(env, num_output);
    } else if (argv[1] == ATOM_parent) {
        (void)ck_spinlock_lock_eb(&h2o_nif_ports_spinlock);
        out = (port->parent == NULL) ? ATOM_undefined : h2o_nif_port_make(env, port->parent);
        (void)ck_spinlock_unlock(&h2o_nif_ports_spinlock);
    } else if (argv[1] == ATOM_state) {
        int state = atomic_load_explicit(&port->state, memory_order_relaxed);
        out = enif_make_int(env, state);
    } else if (argv[1] == ATOM_type) {
        out = enif_make_int(env, enif_make_int(env, port->type));
    } else {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    out = enif_make_tuple2(env, argv[1], out);
    (void)h2o_nif_port_release(port);
    return out;
}

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
        (void)h2o_nif_port_release(port);
        return ATOM_false;
    }
    (void)h2o_nif_port_release(port);
    return ATOM_true;
}

static ERL_NIF_TERM
h2o_nif_port_getopt_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port) || !enif_is_atom(env, argv[1])) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    ERL_NIF_TERM out;
    if (argv[1] == ATOM_active) {
        int active = atomic_load_explicit(&port->active, memory_order_relaxed);
        switch (active) {
        case H2O_NIF_PORT_ACTIVE_FALSE:
            out = ATOM_false;
            break;
        case H2O_NIF_PORT_ACTIVE_ONCE:
            out = ATOM_once;
            break;
        case H2O_NIF_PORT_ACTIVE_TRUE:
            out = ATOM_true;
            break;
        }
    } else {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    out = enif_make_tuple2(env, ATOM_ok, out);
    (void)h2o_nif_port_release(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_setopt_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    h2o_nif_port_t *port = NULL;
    if (argc != 3) {
        return enif_make_badarg(env);
    }
    if (!h2o_nif_port_get(env, argv[0], &port) || !enif_is_atom(env, argv[1])) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (argv[1] == ATOM_active) {
        int expected;
        int desired;
        bool active_desired;
        bool inactive_to_active;
        int retval;
        if (argv[2] == ATOM_false) {
            active_desired = false;
            desired = H2O_NIF_PORT_ACTIVE_FALSE;
        } else if (argv[2] == ATOM_once) {
            active_desired = true;
            desired = H2O_NIF_PORT_ACTIVE_ONCE;
        } else if (argv[2] == ATOM_true) {
            active_desired = true;
            desired = H2O_NIF_PORT_ACTIVE_TRUE;
        } else {
            (void)h2o_nif_port_release(port);
            return enif_make_badarg(env);
        }
        do {
            inactive_to_active = false;
            expected = atomic_load_explicit(&port->active, memory_order_relaxed);
            if (expected == H2O_NIF_PORT_ACTIVE_FALSE && active_desired) {
                inactive_to_active = true;
            }
            // TRACE_F("expected.aba=%d, expected.active=%d, expected.count=%d\n", expected.aba, expected.active, expected.count);
            // TRACE_F("  wanted.aba=%d,   wanted.active=%d,   wanted.count=%d\n", wanted.aba, wanted.active, wanted.count);
            // TRACE_F(" desired.aba=%d,  desired.active=%d,  desired.count=%d\n", desired.aba, desired.active, desired.count);
            retval = atomic_compare_exchange_weak_explicit(&port->active, &expected, desired, memory_order_relaxed,
                                                           memory_order_relaxed);
            if (!retval) {
                (void)ck_pr_stall();
            }
        } while (retval == 0);
        if (inactive_to_active) {
            (void)h2o_nif_port_output_flush(env, port);
        }
    } else {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_release(port);
    return ATOM_ok;
}

static ERL_NIF_TERM
h2o_nif_port_accept_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    // TRACE_F("h2o_nif_port_accept_1:%s:%d\n", __FILE__, __LINE__);
    h2o_nif_port_t *port = NULL;
    if (argc != 1 || !h2o_nif_port_get(env, argv[0], &port)) {
        return enif_make_badarg(env);
    }
    if (h2o_nif_port_is_closed(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_closed);
    }
    if (!h2o_nif_port_is_listening(port)) {
        (void)h2o_nif_port_release(port);
        return enif_make_tuple2(env, ATOM_error, ATOM_eagain);
    }
    ERL_NIF_TERM out;
    if (!h2o_nif_port_accept(env, port, &out)) {
        (void)h2o_nif_port_release(port);
        return enif_make_badarg(env);
    }
    (void)h2o_nif_port_release(port);
    return out;
}

static ERL_NIF_TERM
h2o_nif_port_gc_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 0) {
        return enif_make_badarg(env);
    }
    return (h2o_nif_port_gc_start()) ? ATOM_true : ATOM_false;
}
