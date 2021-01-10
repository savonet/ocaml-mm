0.7.1 (10-01-2021)
=====
* Use only our own custom byte swap implementations.

0.7.0 (04-01-2021)
======
* Switch to dune!
* Add set_alpha
* Add box_alpha

0.6.0 (12-10-2020)
=====
- Use `YUV420` for video frames.
- Use bigarrays to implement mono audio buffers, should be more efficient.
- Add `Image.Generic.blank`.
- Add `scale` and `disk` effects on alpha channel for YUV420.
- Make sure `to_mono` initializes an audio array with zeroes.

0.5.0 (18-08-2019)
=====
* New implementation of `YUV420`, added many function to manipulate those.
* Enhanced `Video` module.

0.4.1 (27-06-2019)
=====
* Fix memory leak in `RGBA32.of_RGB24_string`.
* Add `YUV420.of_string`.

0.4.0 (18-08-2018)
=====
* Use bytes instead of strings whenever appropriate.

0.3.1 (16-10-2017)
=====
* Fixed compilation with OCaml 4.06

0.3.0 (03-08-2015)
=====
* Add support for S16BE, S24LE and S32LE
* Fix deprecated APIs

0.2.1 (2013-02-18)
=====
* Add pulseaudio backend.
* Add channel and rate parameters for AO.
* Add resampling mode (`Nearest or `Linear).
* Remove on-the-fly samplerate conversions which were of bad quality (please
  use a proper resampler such as ocaml-samplerate instead).
* Handle BGRA format.
* Check for memory allocation failures.
* Add a video player example.

0.2.0 (2011-10-04)
=====
* Add alpha channel for drawn lines.
* Improved autoconf.

0.1.0 (2011-06-30)
=====
* Initial release.
