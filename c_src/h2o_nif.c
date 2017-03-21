// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "h2o_nif.h"

/*
 * Erlang NIF functions
 */

#include "h2o_nif/logger.c.h"
#include "h2o_nif/port.c.h"
#include "h2o_nif/server.c.h"
#include "h2o_nif/string.c.h"

/*
 * Erlang NIF callbacks
 */

static int
h2o_nif_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    if (h2o_nif_mutex == NULL) {
        h2o_nif_mutex = enif_mutex_create("h2o_nif_mutex");
    }
    (void)enif_mutex_lock(h2o_nif_mutex);
    if (h2o_nif_rwlock == NULL) {
        h2o_nif_rwlock = enif_rwlock_create("h2o_nif_rwlock");
    }
    h2o_nif_data_t *nif_data = enif_alloc(sizeof(*nif_data));
    if (nif_data == NULL) {
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    nif_data->version = h2o_nif_data_version;
    if (h2o_nif_globals_load(env, nif_data) != 0) {
        (void)enif_free(nif_data);
        (void)enif_mutex_unlock(h2o_nif_mutex);
        return -1;
    }
    *priv_data = (void *)(nif_data);
    (void)enif_mutex_unlock(h2o_nif_mutex);
    return 0;
}

static int
h2o_nif_upgrade(ErlNifEnv *env, void **priv_data, void **old_priv_data, ERL_NIF_TERM load_info)
{
    if (h2o_nif_globals_upgrade(env, priv_data, old_priv_data, load_info) != 0) {
        return -1;
    }
    return 0;
}

static void
h2o_nif_unload(ErlNifEnv *env, void *priv_data)
{
    if (h2o_nif_mutex != NULL) {
        (void)enif_mutex_lock(h2o_nif_mutex);
    }
    h2o_nif_data_t *nif_data = (h2o_nif_data_t *)priv_data;
    (void)h2o_nif_globals_unload(env, nif_data);
    if (priv_data != NULL) {
        (void)enif_free(priv_data);
    }
    if (h2o_nif_mutex != NULL) {
        (void)enif_mutex_unlock(h2o_nif_mutex);
        (void)enif_mutex_destroy(h2o_nif_mutex);
        h2o_nif_mutex = NULL;
    }
}

static ErlNifFunc h2o_nif_funcs[] = {
    // h2o_nif/logger.c.h
    {"logger_read_start", 1, h2o_nif_logger_read_start_1},
    {"logger_read", 1, h2o_nif_logger_read_1},
    // h2o_nif/port.c.h
    {"port_close", 1, h2o_nif_port_close_1},
    {"port_connect", 2, h2o_nif_port_connect_2},
    {"port_info", 1, h2o_nif_port_info_1},
    {"port_info", 2, h2o_nif_port_info_2},
    {"port_is_alive", 1, h2o_nif_port_is_alive_1},
    // h2o_nif/server.c.h
    {"server_open", 0, h2o_nif_server_open_0},
    {"server_getcfg", 1, h2o_nif_server_getcfg_1},
    {"server_setcfg", 2, h2o_nif_server_setcfg_2},
    {"server_start", 1, h2o_nif_server_start_1},
    // h2o_nif/string.c.h
    {"string_tolower", 1, h2o_nif_string_tolower_1},
    {"string_strtolower", 1, h2o_nif_string_strtolower_1},
    {"string_toupper", 1, h2o_nif_string_toupper_1},
    {"string_strtoupper", 1, h2o_nif_string_strtoupper_1},
    {"string_lcstris", 2, h2o_nif_string_lcstris_2},
    {"string_base64_encode_capacity", 1, h2o_nif_string_base64_encode_capacity_1},
    {"string_decode_base64url", 1, h2o_nif_string_decode_base64url_1},
    {"string_base64_encode", 2, h2o_nif_string_base64_encode_2},
    {"string_hex_decode", 1, h2o_nif_string_hex_decode_1},
    {"string_hex_encode", 1, h2o_nif_string_hex_encode_1},
    {"string_uri_escape", 2, h2o_nif_string_uri_escape_2},
    {"string_get_filext", 1, h2o_nif_string_get_filext_1},
    {"string_str_stripws", 1, h2o_nif_string_str_stripws_1},
    {"string_htmlescape", 1, h2o_nif_string_htmlescape_1},
};

ERL_NIF_INIT(h2o_nif, h2o_nif_funcs, h2o_nif_load, NULL, h2o_nif_upgrade, h2o_nif_unload);
