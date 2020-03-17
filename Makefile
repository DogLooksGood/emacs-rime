CC           = gcc
CFLAGS       = -fPIC -g
LDFLAGS      = -shared -lrime
TARGET       = liberime.so

default: clean test

clean:
	rm $(TARGET)

lib:
	$(CC) $(CFLAGS) $(LDFLAGS) src/lib.c -o $(TARGET)

test: clean
	emacs -q test.el -e eval-buffer
