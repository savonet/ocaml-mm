SOURCES = test.ml
RESULT = test
INCDIRS = ../../src ../../external/sdl ../../external/oss ../../../ocaml-gstreamer/src/ +sdl
LIBS = mm sdl mm.sdl mm.oss gstreamer
PACKS = sdl
THREADS = yes
OCAMLNCFLAGS = -p
ANNOTATE = yes

all: pnc

test: all
	OCAMLRUNPARAM=b ./test

gstest:
	#gst-launch -v filesrc location=../test.wmv ! decodebin ! ffmpegcolorspace ! videoscale ! ximagesink
	#gst-launch -v filesrc location=../test.wmv ! decodebin ! ffmpegcolorspace ! videoscale ! videorate ! video/x-raw-rgb,width=320,height=240,framerate='(fraction)'10/1,bpp=32,depth=24 ! ximagesink
	# ! audio/x-raw-int,width=16,channels=2,rate=44100,endianness=4321,signed=true
	gst-launch -v filesrc location=../test.wmv ! decodebin ! audioconvert ! audioresample ! audio/x-raw-int,width=16,channels=2,rate=44100,signed=true ! autoaudiosink

include OCamlMakefile
