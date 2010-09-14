SOURCES = display.ml
RESULT = display
INCDIRS = ../../src
LIBS = str unix bigarray mm graphics

test: all
	OCAMLRUNPARAM=b ./display ../test.ppm

all: pnc

include OCamlMakefile
