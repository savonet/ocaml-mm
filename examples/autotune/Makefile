SOURCES = autotune.ml
RESULT = autotune
INCDIRS = ../../src
LIBS = unix mm
OCAMLFLAGS = -g

test: all
	OCAMLRUNPARAM=b ./autotune

all: dnc

include OCamlMakefile