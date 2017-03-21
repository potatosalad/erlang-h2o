// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_CONFIG_H
#define H2O_NIF_CONFIG_H

#include "globals.h"
#include "port.h"

/* Types */

typedef struct h2o_nif_config_s h2o_nif_config_t;
typedef struct h2o_nif_cfg_listen_s h2o_nif_cfg_listen_t;

struct h2o_nif_cfg_listen_s {
    int fd;
    struct sockaddr_storage addr;
    socklen_t addrlen;
    h2o_hostconf_t **hosts;
    int proxy_protocol;
};

struct h2o_nif_config_s {
    h2o_globalconf_t globalconf;
    h2o_nif_cfg_listen_t **listeners;
    size_t num_listeners;
    char *error_log;
    int error_log_fd;
    int max_connections;
    size_t num_threads;
    int tfo_queues;
    ErlNifEnv *env;
};

/* Config Functions */

extern int h2o_nif_config_init(h2o_nif_config_t *config);
extern void h2o_nif_config_dispose(h2o_nif_config_t *config);
extern int h2o_nif_config_get(h2o_nif_config_t *config, ErlNifEnv *env, ERL_NIF_TERM *out);
extern int h2o_nif_config_set(h2o_nif_config_t *config, ErlNifEnv *env, ErlNifBinary *input, ERL_NIF_TERM *out);

#endif
