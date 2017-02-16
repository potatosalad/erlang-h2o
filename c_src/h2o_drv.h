// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

#ifndef H2O_DRV_H
#define H2O_DRV_H

#include "h2o_drv_common.h"

h2o_drv_term_data_t	*h2o_drv;
ErlDrvMutex		*h2o_mutex;

/*
 * Erlang DRV functions
 */
static int		h2o_drv_init(void);
static ErlDrvData	h2o_drv_start(ErlDrvPort drv_port, char *command);
static void		h2o_drv_stop(ErlDrvData drv_data);
static void		h2o_drv_finish(void);

static ErlDrvEntry	h2o_driver_entry = {
	h2o_drv_init,			/* F_PTR init, called when driver is loaded */
	h2o_drv_start,			/* L_PTR start, called when port is opened */
	h2o_drv_stop,			/* F_PTR stop, called when port is closed */
	NULL,				/* F_PTR output, called when erlang has sent */
	NULL,				/* F_PTR ready_input, called when input descriptor ready */
	NULL,				/* F_PTR ready_output, called when output descriptor ready */
	"h2o_drv",			/* char *driver_name, the argument to open_port */
	h2o_drv_finish,			/* F_PTR finish, called when unloaded */
	NULL,				/* void *handle, Reserved by VM */
	NULL,				/* F_PTR control, port_command callback */
	NULL,				/* F_PTR timeout, reserved */
	NULL,				/* F_PTR outputv, reserved */
	NULL,				/* F_PTR ready_async, only for async drivers */
	NULL,				/* F_PTR flush, called when port is about to be closed, but there is data in driver queue */
	NULL,				/* F_PTR call, much like control, sync call to driver */
	NULL,				/* F_PTR event, called when an event selected by driver_event() occurs. */
	ERL_DRV_EXTENDED_MARKER,	/* int extended marker, Should always be set to indicate driver versioning */
	ERL_DRV_EXTENDED_MAJOR_VERSION,	/* int major_version, should always be set to this value */
	ERL_DRV_EXTENDED_MINOR_VERSION,	/* int minor_version, should always be set to this value */
	ERL_DRV_FLAG_USE_PORT_LOCKING,	/* int driver_flags, see documentation */
	NULL,				/* void *handle2, reserved for VM use */
	NULL,				/* F_PTR process_exit, called when a monitored process dies */
	NULL				/* F_PTR stop_select, called to close an event object */
};

#endif