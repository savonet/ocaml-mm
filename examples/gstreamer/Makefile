SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/oss ../../../ocaml-gstreamer/src/
LIBS = bigarray mm mm.oss gstreamer
THREADS = yes
OCAMLNCFLAGS = -p

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

include OCamlMakefile
