SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/oss ../../../ocaml-gstreamer/src/
LIBS = unix bigarray mm mm.oss gstreamer

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

include OCamlMakefile
