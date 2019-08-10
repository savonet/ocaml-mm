SOURCES = player.ml
RESULT = player
OCAMLFLAGS = -g
INCDIRS = ../../src ../../external/v4l ../../external/sdl +sdl
LIBS = str unix bigarray graphics mm mm.v4l sdl mm.sdl

test: dnc
	OCAMLRUNPARAM=b ./player

all: nc

prof:
	$(MAKE) clean pnc test
	gprof ./player > prof
	head -n 20 prof

gdb: dnc
	gdb --args ./player ../test.mov

valgrind: dnc
	valgrind ./player ../test.mov

include OCamlMakefile

.PHONY: prof
