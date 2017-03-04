// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "globals.h"

ErlNifMutex *h2o_nif_mutex = NULL;
ErlNifRWLock *h2o_nif_rwlock = NULL;
h2o_sem_t h2o_ocsp_updater_semaphore;
khash_t(h2o_nif_port_t) *h2o_nif_ports = NULL;
volatile sig_atomic_t h2o_nif_num_ports = 0;
volatile sig_atomic_t h2o_nif_seq_ports = 0;
