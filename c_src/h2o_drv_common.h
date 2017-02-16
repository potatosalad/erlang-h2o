// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

#ifndef H2O_DRV_COMMON_H
#define H2O_DRV_COMMON_H

#include <sys/types.h>
#include <erl_driver.h>
#include <erl_interface.h>
#include <unistd.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>

extern int	erts_fprintf(FILE *, const char *, ...);

// #define TRACE	1
#ifdef TRACE
	#define TRACE_C(c)	do { putchar(c); fflush(stdout); } while (0)
	#define TRACE_S(s)	do { fputs((s), stdout); fflush(stdout); } while (0)
	#define TRACE_F(...)	erts_fprintf(stderr, __VA_ARGS__)
#else
	#define TRACE_C(c)	((void)(0))
	#define TRACE_S(s)	((void)(0))
	#define TRACE_F(...)	((void)(0))
#endif

/* common */
typedef struct h2o_drv_term_data {
	ErlDrvTermData	am_ok;
	ErlDrvTermData	am_error;
	ErlDrvTermData	am_undefined;
} h2o_drv_term_data_t;

extern h2o_drv_term_data_t	*h2o_drv;
extern ErlDrvMutex		*h2o_mutex;

#define H2O_ATOM(NAME)		(ErlDrvTermData)(h2o_drv->am_ ## NAME)
#define H2O_STRING(NAME)	(char *)(h2o_drv->str_ ## NAME)

#define H2O_FAIL_BADSPEC(PORT)	(void)(driver_failure_atom(PORT, "bad_spec"))
#define H2O_FAIL_OOM(PORT)	(void)(driver_failure_atom(PORT, "out_of_memory"))

#endif