// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "globals.h"

ErlNifMutex *h2o_nif_mutex = NULL;
h2o_sem_t h2o_ocsp_updater_semaphore;
