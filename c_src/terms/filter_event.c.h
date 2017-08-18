// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "../records.h"

ERL_NIF_TERM
h2o_nif_filter_event_make(ErlNifEnv *env, h2o_nif_filter_event_t *filter_event)
{
    TRACE_F("h2o_nif_filter_event_make:%s:%d\n", __FILE__, __LINE__);
    ERL_NIF_TERM out;
    out = h2o_nif_proto_req_make(env, &filter_event->proto);
    return out;
}

ERL_NIF_TERM
h2o_nif_filter_event_ientry_make(ErlNifEnv *env, h2o_nif_filter_event_ientry_t *fi)
{
    TRACE_F("h2o_nif_filter_event_ientry_make:%s:%d\n", __FILE__, __LINE__);
    ERL_NIF_TERM out;
    // DEBUG_F("enif_make_binary(%llu):%s:%d\n", fi->binary.size, __FILE__, __LINE__);
    out = enif_make_binary(env, &fi->binary);
    return out;
}
