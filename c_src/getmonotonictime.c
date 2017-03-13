// -*- mode: c; tab-width: 4; indent-tabs-mode: nil; st-rulers: [132] -*-
// vim: ts=4 sw=4 ft=c et

#include "getmonotonictime.h"

#include <mach/mach.h>
#include <mach/clock.h>
#include <mach/mach_time.h>

// #define MT_NANO (+1.0E-9)
// #define MT_GIGA UINT64_C(1000000000)

// // TODO create a list of timers,
// static double mt_timebase = 0.0;
// static uint64_t mt_timestart = 0;

int
getmonotonictime(struct timespec *tp)
{
    kern_return_t retval = KERN_SUCCESS;
    clock_serv_t cclock;
    mach_timespec_t mts;

    host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
    retval = clock_get_time(cclock, &mts);
    mach_port_deallocate(mach_task_self(), cclock);

    tp->tv_sec = mts.tv_sec;
    tp->tv_nsec = mts.tv_nsec;

    return (int)retval;
}
