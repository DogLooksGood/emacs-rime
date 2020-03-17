CC           = gcc
CFLAGS       = -fPIC -g
LDFLAGS      = -shared -lrime
TARGET       = emacs-rime-lib.so

default: lib test

lib:
	$(CC) $(CFLAGS) $(LDFLAGS) src/lib.c -o $(TARGET)

test:
	emacs -Q test.el
