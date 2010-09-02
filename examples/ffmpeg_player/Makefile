SOURCES = player.ml
RESULT = player
OCAMLFLAGS = -g
INCDIRS = ../../src ../../external/ffmpeg
LIBS = str unix bigarray graphics mm mm.ffmpeg

test: dnc
	OCAMLRUNPARAM=b ./player ../test.mov

include OCamlMakefile