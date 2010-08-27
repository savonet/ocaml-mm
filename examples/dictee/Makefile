SOURCES = dictee.ml
RESULT = dictee
INCDIRS = ../../src ../../external +mad
LIBS = unix mad mm mm-ext
OCAMLFLAGS = -g

test: all
	OCAMLRUNPARAM=b ./dictee ../test.mp3

all: dnc

include OCamlMakefile