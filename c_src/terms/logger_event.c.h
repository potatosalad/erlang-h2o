// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

ERL_NIF_TERM
h2o_nif_logger_event_make(ErlNifEnv *env, h2o_nif_logger_event_t *logger_event)
{
    TRACE_F("h2o_nif_logger_event_make:%s:%d\n", __FILE__, __LINE__);
    ERL_NIF_TERM out;
    out = enif_make_binary(env, &logger_event->binary);
    return out;
}
