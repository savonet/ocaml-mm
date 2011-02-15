SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/oss ../../../ocaml-gstreamer/src/
LIBS = unix bigarray mm mm.oss gstreamer
OCAMLNCFLAGS = -p

test: all
	OCAMLRUNPARAM=b ./test

all: pnc

gstlaunch:
	gst-launch -v filesrc location=../test.wav ! decodebin ! audio/x-raw-int,channels=2,rate=44100 ! alsasink

include OCamlMakefile
