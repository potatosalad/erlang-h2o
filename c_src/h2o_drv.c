// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

#include "h2o_drv.h"
#include "h2o_port.h"

#define INIT_ATOM(NAME)		h2o_drv->am_ ## NAME = driver_mk_atom(#NAME)

/*
 * Erlang DRV functions
 */
static int
h2o_drv_init(void)
{
	TRACE_F("h2o_drv_init:%s:%d\n", __FILE__, __LINE__);

	if (h2o_mutex == NULL) {
		h2o_mutex = erl_drv_mutex_create("h2o");
		if (h2o_mutex == NULL) {
			return -1;
		}
	}

	(void) erl_drv_mutex_lock(h2o_mutex);

	if (h2o_drv == NULL) {
		h2o_drv = (h2o_drv_term_data_t *)(driver_alloc(sizeof (h2o_drv_term_data_t)));
		if (h2o_drv == NULL) {
			(void) erl_drv_mutex_unlock(h2o_mutex);
			return -1;
		}
		INIT_ATOM(ok);
		INIT_ATOM(error);
		INIT_ATOM(undefined);
	}

	(void) erl_drv_mutex_unlock(h2o_mutex);

	return 0;
}

static ErlDrvData
h2o_drv_start(ErlDrvPort drv_port, char *command)
{
	h2o_port_t *port;

	(void) command; // Unused

	TRACE_F("h2o_drv_start:%s:%d\n", __FILE__, __LINE__);

	port = h2o_port_alloc(drv_port);

	if (port == NULL) {
		return ERL_DRV_ERROR_GENERAL;
	}

	return (ErlDrvData)(port);
}

static void
h2o_drv_stop(ErlDrvData drv_data)
{
	h2o_port_t *port;

	TRACE_F("h2o_drv_stop:%s:%d\n", __FILE__, __LINE__);

	port = (h2o_port_t *)(drv_data);

	(void) h2o_port_free(port);
}

static void
h2o_drv_finish(void)
{
	TRACE_F("h2o_drv_finish:%s:%d\n", __FILE__, __LINE__);
	if (h2o_mutex != NULL) {
		(void) erl_drv_mutex_lock(h2o_mutex);
	}
	if (h2o_drv != NULL) {
		(void) driver_free(h2o_drv);
		h2o_drv = NULL;
	}
	if (h2o_mutex != NULL) {
		(void) erl_drv_mutex_unlock(h2o_mutex);
		(void) erl_drv_mutex_destroy(h2o_mutex);
		h2o_mutex = NULL;
	}
}

DRIVER_INIT(h2o_drv)
{
	return &h2o_driver_entry;
}
