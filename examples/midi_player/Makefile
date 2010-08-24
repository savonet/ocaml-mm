SOURCES = player.ml
RESULT = player
INCDIRS = ../../src
LIBS = unix mm

all: pnc

test: all
	OCAMLRUNPARAM=b ./player ../test.mid

prof: test
	gprof ./player > prof

include OCamlMakefile
