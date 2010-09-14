SOURCES = fft.ml
RESULT = fft
INCDIRS = ../../src ../../external/oss
LIBS = unix bigarray mm graphics mm.oss

all: dnc

test: all
	OCAMLRUNPARAM=b ./fft ../test.wav

include OCamlMakefile
