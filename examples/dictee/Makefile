SOURCES = dictee.ml
RESULT = dictee
INCDIRS = ../../src ../../external/oss ../../external/mad +mad
LIBS = unix bigarray mad mm mm.oss mm.mad
OCAMLFLAGS = -g

test: all
	OCAMLRUNPARAM=b ./dictee ../test.mp3

all: dnc

include OCamlMakefile
