SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/oss ../../../ocaml-gstreamer/src/
LIBS = bigarray mm mm.oss gstreamer
THREADS = yes
OCAMLNCFLAGS = -p

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

gstlaunch:
	gst-launch -v filesrc location=../test.wav ! decodebin ! audio/x-raw-int,channels=2,rate=44100 ! alsasink

include OCamlMakefile
