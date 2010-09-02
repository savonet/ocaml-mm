SOURCES = player.ml
RESULT = player
OCAMLFLAGS = -g
INCDIRS = ../../src ../../external/theora +ogg +theora
LIBS = str unix bigarray mm ogg theora mm.theora

test: dnc
	OCAMLRUNPARAM=b ./player ../test.ogv

include OCamlMakefile