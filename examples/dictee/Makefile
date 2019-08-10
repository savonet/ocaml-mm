SOURCES = dictee.ml
RESULT = dictee
INCDIRS = ../../src ../../external/oss ../../external/mad
LIBS = unix bigarray mad mm mm.oss mm.mad
OCAMLFLAGS = -g

all: dnc

test: all
	OCAMLRUNPARAM=b ./dictee ../test.mp3

include OCamlMakefile
include ../../external/Makefile.defs
