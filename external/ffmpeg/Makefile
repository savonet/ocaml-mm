SOURCES = ffmpeg_stubs.c MMFFmpeg.mli MMFFmpeg.ml
RESULT = mm.ffmpeg
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g
OCAMLNCFLAGS = -p
CFLAGS = -g
CLIBS = avutil avformat avcodec swscale

all: ncl

include OCamlMakefile
include ../Makefile.defs
INCDIRS += ../../src