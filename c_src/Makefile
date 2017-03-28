# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT = h2o

# Configuration.

C_SRC_DIR ?= $(CURDIR)
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk
C_SRC_OUTPUT ?= $(BASEDIR)/priv/$(PROJECT)_nif
C_SRC_TYPE ?= shared

C_DEPS_DIR ?= $(BASEDIR)/c_deps

# "erl" command.

ERL = erl +A0 -noinput -boot start_clean

# Platform detection.

ifeq ($(PLATFORM),)
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
PLATFORM = linux
else ifeq ($(UNAME_S),Darwin)
PLATFORM = darwin
else ifeq ($(UNAME_S),SunOS)
PLATFORM = solaris
else ifeq ($(UNAME_S),GNU)
PLATFORM = gnu
else ifeq ($(UNAME_S),FreeBSD)
PLATFORM = freebsd
else ifeq ($(UNAME_S),NetBSD)
PLATFORM = netbsd
else ifeq ($(UNAME_S),OpenBSD)
PLATFORM = openbsd
else ifeq ($(UNAME_S),DragonFly)
PLATFORM = dragonfly
else ifeq ($(shell uname -o),Msys)
PLATFORM = msys2
else
$(error Unable to detect platform. Please open a ticket with the output of uname -a.)
endif

export PLATFORM
endif

# System type and C compiler/flags.

ifeq ($(PLATFORM),msys2)
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?= .exe
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .dll
else
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?=
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .so
endif

ifeq ($(C_SRC_TYPE),shared)
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_SHARED_EXTENSION)
else
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_EXECUTABLE_EXTENSION)
endif

ifeq ($(PLATFORM),msys2)
# We hardcode the compiler used on MSYS2. The default CC=cc does
# not produce working code. The "gcc" MSYS2 package also doesn't.
	CC = /mingw64/bin/gcc
	CFLAGS ?= -O3 -std=c11 -finline-functions -fstack-protector -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -fstack-protector -Wall
else ifeq ($(PLATFORM),darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c11 -arch x86_64 -fstack-protector -Wall -Wmissing-prototypes
	# CFLAGS ?= -O3 -std=c11 -arch x86_64 -fstack-protector -Wall -Wmissing-prototypes -pedantic -Weverything -Wextra -Wno-unused-parameter -Wno-gnu
	CXXFLAGS ?= -O3 -arch x86_64 -fstack-protector -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(PLATFORM),freebsd)
	CC ?= cc
	CFLAGS ?= -O3 -std=c11 -finline-functions -fstack-protector -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -fstack-protector -Wall
else ifeq ($(PLATFORM),linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c11 -finline-functions -fstack-protector -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -fstack-protector -Wall
else ifeq ($(PLATFORM),solaris)
	CC ?= cc
	CFLAGS ?= -O3 -std=c11 -finline-functions -fstack-protector -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O3 -finline-functions -fstack-protector -Wall
endif

ifneq ($(PLATFORM),msys2)
	CFLAGS += -fPIC
	CXXFLAGS += -fPIC
endif

CFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)" \
	-DH2O_USE_LIBUV=0 \
	-I"$(CK_INCLUDE_DIR)" \
	-I"$(H2O_INCLUDE_DIR)" \
	-I"$(KLIB_INCLUDE_DIR)" \
	-I"$(LIBRESSL_INCLUDE_DIR)" \
	-I"$(WSLAY_INCLUDE_DIR)" \
	-I"$(WSLAY_LIBINCLUDE_DIR)" \
	-I"$(YAML_INCLUDE_DIR)" \
	-I"$(YOML_INCLUDE_DIR)"
CXXFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)" \
	-DH2O_USE_LIBUV=0 \
	-I"$(CK_INCLUDE_DIR)" \
	-I"$(H2O_INCLUDE_DIR)" \
	-I"$(KLIB_INCLUDE_DIR)" \
	-I"$(LIBRESSL_INCLUDE_DIR)" \
	-I"$(WSLAY_INCLUDE_DIR)" \
	-I"$(WSLAY_LIBINCLUDE_DIR)" \
	-I"$(YAML_INCLUDE_DIR)" \
	-I"$(YOML_INCLUDE_DIR)"

# LDLIBS += -L"$(ERL_INTERFACE_LIB_DIR)" -lerl_interface -lei -lz "$(WSLAY_OUTPUT_FILE)" "$(H2O_OUTPUT_FILE)"
LDLIBS += -L"$(ERL_INTERFACE_LIB_DIR)" -lerl_interface -lei -lz \
	-L"$(CK_LIB_DIR)" -lck \
	-L"$(H2O_LIB_DIR)" -lh2o-evloop \
	-L"$(LIBRESSL_LIB_DIR)" -lssl -lcrypto \
	-L"$(WSLAY_LIB_DIR)" -lwslay \
	-L"$(YAML_LIB_DIR)" -lyaml

# Dependencies.

CK_VSN ?= 0.6.0
CK_GIT ?= git://github.com/concurrencykit/ck.git
CK_SRC_DIR ?= $(C_DEPS_DIR)/ck
CK_OUTPUT_FILE ?= $(CK_SRC_DIR)/src/libck.a
CK_LIB_DIR ?= $(CK_SRC_DIR)/src
CK_INCLUDE_DIR ?= $(CK_SRC_DIR)/include
CK_MAKE_FILE ?= $(CK_SRC_DIR)/Makefile

H2O_VSN ?= v2.2.0-beta2-20170315
H2O_GIT ?= git://github.com/potatosalad/h2o.git
H2O_SRC_DIR ?= $(C_DEPS_DIR)/h2o
H2O_BUILD_DIR ?= $(C_DEPS_DIR)/h2o-build
H2O_OUTPUT_FILE ?= $(H2O_BUILD_DIR)/libh2o-evloop.a
H2O_LIB_DIR ?= $(H2O_BUILD_DIR)
H2O_INCLUDE_DIR ?= $(H2O_SRC_DIR)/include
H2O_MAKE_FILE ?= $(H2O_BUILD_DIR)/Makefile

KLIB_INCLUDE_DIR ?= $(H2O_SRC_DIR)/deps/klib

LIBRESSL_LIB_DIR ?= $(H2O_BUILD_DIR)/libressl-build/lib
LIBRESSL_INCLUDE_DIR ?= $(H2O_BUILD_DIR)/libressl-build/include

WSLAY_VSN ?= 6abacc1da10344d7d7b13c21edc76fcdf587de0c
WSLAY_GIT ?= git://github.com/tatsuhiro-t/wslay.git
WSLAY_SRC_DIR ?= $(C_DEPS_DIR)/wslay
WSLAY_BUILD_DIR ?= $(C_DEPS_DIR)/wslay-build
WSLAY_LIB_DIR ?= $(WSLAY_BUILD_DIR)/lib
WSLAY_LIBINCLUDE_DIR ?= $(WSLAY_LIB_DIR)/includes
WSLAY_INCLUDE_DIR ?= $(WSLAY_SRC_DIR)/lib/includes
WSLAY_OUTPUT_FILE ?= $(WSLAY_LIB_DIR)/libwslay.a
WSLAY_MAKE_FILE ?= $(WSLAY_BUILD_DIR)/Makefile

YAML_SRC_DIR ?= $(H2O_SRC_DIR)/deps/yaml
YAML_BUILD_DIR ?= $(C_DEPS_DIR)/yaml-build
YAML_LIB_DIR ?= $(YAML_BUILD_DIR)
YAML_INCLUDE_DIR ?= $(YAML_SRC_DIR)/include
YAML_OUTPUT_FILE ?= $(YAML_LIB_DIR)/libyaml.a
YAML_MAKE_FILE ?= $(YAML_BUILD_DIR)/Makefile

YOML_INCLUDE_DIR ?= $(H2O_SRC_DIR)/deps/yoml

ALL_DEPS_OUTPUT ?= $(CK_OUTPUT_FILE) $(WSLAY_OUTPUT_FILE) $(H2O_OUTPUT_FILE) $(YAML_OUTPUT_FILE)

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

dep_verbose_0 = @echo " DEP   " $(1);
dep_verbose_2 = set -x;
dep_verbose = $(dep_verbose_$(V))

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose_2 = set -x;
gen_verbose = $(gen_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Targets.

core_find = $(if $(wildcard $1),$(shell find $(1:%/=%) -type f -name $(subst *,\*,$2)))

ifeq ($(PLATFORM),msys2)
core_native_path = $(subst \,\\\\,$(shell cygpath -w $1))
else
core_native_path = $1
endif

ifeq ($(SOURCES),)
SOURCES := $(sort $(foreach pat,*.c *.C *.cc *.cpp,$(call core_find,$(C_SRC_DIR)/,$(pat))))
endif
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

all:: deps app

app:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

ifeq ($(filter $(foreach p,$(ALL_DEPS_OUTPUT),$(if $(wildcard $(p)),,x)), x),)

deps::

else

deps:: $(CK_OUTPUT_FILE) $(WSLAY_OUTPUT_FILE) $(H2O_OUTPUT_FILE) $(YAML_OUTPUT_FILE)

endif

test-build:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

$(C_DEPS_DIR):
	$(verbose) mkdir -p $(C_DEPS_DIR)

$(CK_SRC_DIR):
	$(verbose) if [ ! -f $(CK_SRC_DIR)/configure ]; then \
		git clone -n $(CK_GIT) $(CK_SRC_DIR) && \
		(cd $(CK_SRC_DIR) && git checkout -q $(CK_VSN)); \
	else \
		true; \
	fi

$(CK_MAKE_FILE): $(CK_SRC_DIR)
	$(verbose) if [ ! -f $(CK_MAKE_FILE) ]; then \
		(cd $(CK_SRC_DIR) \
			&& env -i ./configure); \
	else \
		true; \
	fi

$(CK_OUTPUT_FILE): $(CK_MAKE_FILE)
	$(MAKE) -C $(CK_SRC_DIR)
	$(verbose) rm -f $(CK_SRC_DIR)/src/libck.so

$(H2O_SRC_DIR):
	$(verbose) if [ ! -f $(H2O_SRC_DIR)/CMakeLists.txt ]; then \
		git clone -n $(H2O_GIT) $(H2O_SRC_DIR) && \
		(cd $(H2O_SRC_DIR) && git checkout -q $(H2O_VSN)); \
	else \
		true; \
	fi

$(H2O_MAKE_FILE): $(H2O_SRC_DIR)
	$(verbose) mkdir -p $(H2O_BUILD_DIR)
	$(verbose) if [ ! -f $(H2O_MAKE_FILE) ]; then \
		(cd $(H2O_BUILD_DIR) \
			&& cmake \
			-DWSLAY_LIBRARIES="$(WSLAY_LIB_DIR)" \
			-DWSLAY_INCLUDE_DIR="$(WSLAY_INCLUDE_DIR)" \
			-DWITH_BUNDLED_SSL_FOR_LIBH2O=on \
			-DWITH_MRUBY=off \
			-DWITH_PICOTLS=on \
			"$(H2O_SRC_DIR)"); \
	else \
		true; \
	fi

$(H2O_OUTPUT_FILE): $(H2O_MAKE_FILE)
	$(MAKE) -C $(H2O_BUILD_DIR) libh2o-evloop

$(WSLAY_SRC_DIR):
	$(verbose) if [ ! -f $(WSLAY_SRC_DIR)/CMakeLists.txt ]; then \
		git clone -n $(WSLAY_GIT) $(WSLAY_SRC_DIR) && \
		(cd $(WSLAY_SRC_DIR) && git checkout -q $(WSLAY_VSN)); \
	else \
		true; \
	fi

$(WSLAY_MAKE_FILE): $(WSLAY_SRC_DIR)
	$(verbose) mkdir -p $(WSLAY_BUILD_DIR)
	$(verbose) if [ ! -f $(WSLAY_MAKE_FILE) ]; then \
		(cd $(WSLAY_BUILD_DIR) && cmake "$(WSLAY_SRC_DIR)"); \
	else \
		true; \
	fi

$(WSLAY_OUTPUT_FILE): $(WSLAY_MAKE_FILE)
	$(MAKE) -C $(WSLAY_BUILD_DIR)

$(YAML_MAKE_FILE): $(H2O_SRC_DIR)
	$(verbose) mkdir -p $(YAML_BUILD_DIR)
	$(verbose) if [ ! -f $(YAML_MAKE_FILE) ]; then \
		(cd $(YAML_BUILD_DIR) && cmake "$(YAML_SRC_DIR)"); \
	else \
		true; \
	fi

$(YAML_OUTPUT_FILE): $(YAML_MAKE_FILE)
	$(MAKE) -C $(YAML_BUILD_DIR)

$(C_SRC_OUTPUT_FILE): $(OBJECTS)
	$(verbose) mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) \
		$(LDFLAGS) $(if $(filter $(C_SRC_TYPE),shared),-shared) $(LDLIBS) \
		-o $(C_SRC_OUTPUT_FILE)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:: clean-c_src

clean-c_src:
	$(gen_verbose) rm -f $(C_SRC_OUTPUT_FILE) $(OBJECTS)

$(C_SRC_ENV):
	$(verbose) $(ERL) -eval "file:write_file(\"$(call core_native_path,$(C_SRC_ENV))\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt()."

distclean:: distclean-env distclean-deps

distclean-env:
	$(gen_verbose) rm -f $(C_SRC_ENV)

distclean-deps:
	$(gen_verbose) rm -rf $(C_DEPS_DIR)

clang-format-all:
	$(gen_verbose) clang-format -i *.c *.h batch/*.c.h h2o_nif/*.c.h

-include $(C_SRC_ENV)
