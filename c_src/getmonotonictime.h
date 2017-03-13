// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#ifndef H2O_NIF_GETMONOTONICTIME_H
#define H2O_NIF_GETMONOTONICTIME_H

#include <sys/types.h>
#include <sys/_types/_timespec.h>

extern int getmonotonictime(struct timespec *tp);

#endif
