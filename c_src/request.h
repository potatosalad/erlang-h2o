// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_REQUEST_H
#define H2O_NIF_REQUEST_H

#include "globals.h"

struct h2o_nif_request_s {
    h2o_nif_handler_t *handler;
    h2o_req_t *req;
};

extern h2o_nif_request_t *h2o_nif_request_alloc(h2o_nif_handler_t *handler);
extern h2o_nif_request_t *h2o_nif_request_register(h2o_nif_handler_t *handler, h2o_req_t *req);
extern void h2o_nif_request_dtor(ErlNifEnv *env, void *obj);

#endif