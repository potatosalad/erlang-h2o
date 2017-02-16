// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

#ifndef H2O_PORT_H
#define H2O_PORT_H

#include "h2o_drv_common.h"

typedef struct h2o_port {
	ErlDrvPort	drv_port;
	ErlDrvTermData	term_port;
} h2o_port_t;

extern h2o_port_t	*h2o_port_alloc(ErlDrvPort drv_port);
extern void		h2o_port_free(h2o_port_t *port);

#endif