SOURCES = gui.glade player.ml
RESULT = player
OCAMLFLAGS = -g
INCDIRS = ../../src ../../external/ffmpeg +lablgtk2
LIBS = str bigarray graphics mm mm.ffmpeg lablglade
THREADS = yes
OCAMLBLDFLAGS = lablgtk.cma gtkInit.cmo gtkThread.cmo
OCAMLNLDFLAGS = lablgtk.cmxa gtkInit.cmx gtkThread.cmx

test: dnc
	OCAMLRUNPARAM=b ./player ../test.mov

include OCamlMakefile