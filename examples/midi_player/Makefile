SOURCES = player.ml
RESULT = player
INCDIRS = ../../src ../../external/oss
LIBS = unix bigarray mm mm.oss

all: pnc

test: all
	OCAMLRUNPARAM=b ./player ../test.mid

prof: test
	gprof ./player > prof

include OCamlMakefile
