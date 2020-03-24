TARGET       = librime-emacs$(LIBRIME_DYLIB)

CC           = gcc

ifdef DEBUG
	CFLAGS   = -fPIC -g -Wall
else
	CFLAGS   = -fPIC -O2 -Wall
endif

ifeq ($(LIBRIME_DYLIB), .so)
	LDFLAGS = -shared
endif

ifeq ($(LIBRIME_DYLIB), .dylib)
	LDFLAGS = -dynamiclib
endif

ifdef LIBRIME_ROOT
	ENV      = C_INCLUDE_PATH=${LIBRIME_ROOT}/include/
	LDFLAGS  += -L ${LIBRIME_ROOT}/lib/ -Wl,-rpath ${LIBRIME_ROOT}/lib/ -lrime
else
	ENV      =
	LDFLAGS  += -lrime
endif

ifdef EMACS_MODULE_HEADER_ROOT
	CFLAGS += -I ${EMACS_MODULE_HEADER_ROOT}
endif

default: clean test

clean:
	-rm $(TARGET)

lib:
	$(ENV) $(CC) lib.c -o $(TARGET) $(CFLAGS) $(LDFLAGS)

test:
	emacs -q test.el -e eval-buffer
