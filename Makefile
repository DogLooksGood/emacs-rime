ifdef MODULE_FILE_SUFFIX
	SUFFIX = $(MODULE_FILE_SUFFIX)
else
	SUFFIX = .so
endif

TARGET       = librime-emacs$(SUFFIX)

CC           = gcc

ifdef DEBUG
	CFLAGS   = -fPIC -g -Wall
else
	CFLAGS   = -fPIC -O2 -Wall
endif

ifeq ($(SUFFIX), .dylib)
	LDFLAGS = -dynamiclib
else
	LDFLAGS = -shared
endif

ifdef LIBRIME_ROOT
	ENV      = env C_INCLUDE_PATH=${LIBRIME_ROOT}include/
	LDFLAGS  += -L ${LIBRIME_ROOT}lib/ -Wl,-rpath ${LIBRIME_ROOT}lib/ -lrime
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
