SOURCES = player.ml
RESULT = player
INCDIRS = ../../src
LIBS = unix mm

all: dnc

test: all
	OCAMLRUNPARAM=b ./player ../test.wav

include OCamlMakefile
