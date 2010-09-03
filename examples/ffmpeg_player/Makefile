SOURCES = player.ml
RESULT = player
OCAMLFLAGS = -g
INCDIRS = ../../src ../../external/ffmpeg ../../external/sdl +sdl
LIBS = str unix bigarray graphics mm mm.ffmpeg sdl mm.sdl

test: dnc
	OCAMLRUNPARAM=b ./player ../test.mov

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