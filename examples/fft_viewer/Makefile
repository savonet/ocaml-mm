SOURCES = fft.ml
RESULT = fft
INCDIRS = ../../src
LIBS = unix mm graphics

all: dnc

test: all
	OCAMLRUNPARAM=b ./fft ../test.wav

include OCamlMakefile
