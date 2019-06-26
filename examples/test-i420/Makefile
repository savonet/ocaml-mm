SOURCES = test.ml
RESULT = test
INCDIRS = ../../src
LIBS = unix bigarray graphics mm

all: nc

test: all
	./test

valgrind: all
	valgrind --leak-check=full ./test

include OCamlMakefile
