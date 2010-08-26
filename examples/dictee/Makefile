SOURCES = dictee.ml
RESULT = dictee
INCDIRS = ../../src ../../external +mad
LIBS = unix mad mm mm-ext

test: all
	OCAMLRUNPARAM=b ./dictee ../test.mp3

all: dnc

include OCamlMakefile