// -*- mode: c; tab-width: 8; indent-tabs-mode: 1; st-rulers: [70] -*-
// vim: ts=8 sw=8 ft=c noet

#include "h2o_port.h"

h2o_port_t *
h2o_port_alloc(ErlDrvPort drv_port)
{
	ErlDrvSizeT x;
	void *p;
	h2o_port_t *port;

	x = (ErlDrvSizeT)((sizeof (h2o_port_t)));
	p = driver_alloc(x);

	if (p == NULL) {
		return NULL;
	}

	port = (h2o_port_t *)(p);
	port->drv_port = drv_port;
	port->term_port = driver_mk_port(drv_port);

	return port;
}

void
h2o_port_free(h2o_port_t *port)
{
	if (port == NULL) {
		return;
	}
	(void) driver_free(port);
	port = NULL;
}
