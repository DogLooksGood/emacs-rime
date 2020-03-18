TARGET       = librime-emacs.so

CC           = gcc
CFLAGS       = -fPIC -g
LDFLAGS      = -shared

ifdef LIBRIME_ROOT
	ENV      = C_INCLUDE_PATH=${LIBRIME_ROOT}dist/include/
	LDFLAGS  = -shared -L${LIBRIME_ROOT}dist/lib -Wl,-rpath=${LIBRIME_ROOT}dist/lib -lrime
else
	ENV      =
    LDFLAGS  = -shared -lrime
endif

default: clean test

clean:
	-rm $(TARGET)

lib:
	$(ENV) $(CC) $(CFLAGS) $(LDFLAGS) lib.c -o $(TARGET)

test:
	emacs -q test.el -e eval-buffer
