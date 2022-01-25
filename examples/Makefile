all: build

build:
	@dune build

autotune: build
	dune exec ./autotune.exe

dictee: build
	dune exec ./dictee.exe test.mp3

display: build
	dune exec ./display.exe test.ppm

drums: build
	dune exec ./drums.exe

fft: build
	dune exec ./fft.exe test.wav

id: build
	dune exec ./id.exe test.wav

midiplayer: build
	dune exec ./midiplayer.exe test.mid

sine_wav: build
	dune exec ./sine_wav.exe

test:
	@dune exec ./test.exe
