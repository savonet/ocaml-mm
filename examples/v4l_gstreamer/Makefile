SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/sdl ../../../ocaml-gstreamer/src/ +sdl
LIBS = unix bigarray mm sdl mm.sdl gstreamer
OCAMLNCFLAGS = -p

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

gstest:
	gst-launch -v videotestsrc ! ffmpegcolorspace ! videoscale ! video/x-raw-rgb,width=320,height=240,framerate='(fraction)'10/1,bpp=32,depth=24 ! ximagesink

include OCamlMakefile
