SOURCES = autotune.ml
RESULT = autotune
INCDIRS = ../../src ../../external/alsa +alsa
LIBS = unix bigarray mm alsa mm.alsa
OCAMLFLAGS = -g

test: all
	OCAMLRUNPARAM=b ./autotune

all: dnc

include OCamlMakefile
