SOURCES = sine_wav.ml
RESULT = sine_wav
INCDIRS = $(shell ocamlfind query ao) ../../src ../../external/ao
LIBS = unix mm ao mm.ao

all: nc

test: all
	./sine_wav

include OCamlMakefile
