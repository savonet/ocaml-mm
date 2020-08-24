SOURCES = test.ml
RESULT = test
INCDIRS = ../../src
LIBS = mm
PACKS = unix bigarray graphics

all: nc

test: all
	./test

valgrind: all
	valgrind --leak-check=full ./test

include OCamlMakefile
