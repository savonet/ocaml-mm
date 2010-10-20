SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/sdl ../../../ocaml-gstreamer/src/ +sdl
LIBS = unix bigarray mm sdl mm.sdl gstreamer
OCAMLNCFLAGS = -p

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

include OCamlMakefile
