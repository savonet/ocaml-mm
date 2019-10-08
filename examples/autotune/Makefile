SOURCES = autotune.ml
RESULT = autotune
INCDIRS = ../../src ../../external/alsa ../../../ocaml-alsa/src
LIBS = unix bigarray alsa mm mm.alsa
OCAMLFLAGS = -g

test: all
	OCAMLRUNPARAM=b ./autotune

all: dnc

include OCamlMakefile
