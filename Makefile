CC           = gcc
CFLAGS       = -fPIC -g
LDFLAGS      = -shared
TARGET       = emacs-rime-lib.so

default: lib

lib:
	$(CC) $(CFLAGS) $(LDFLAGS) src/lib.c -o $(TARGET)
