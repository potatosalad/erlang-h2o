PROJECT = h2o
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = ranch

include erlang.mk

distclean:: distclean-c_src

distclean-c_src:
	$(MAKE) -C $(C_SRC_DIR) distclean
