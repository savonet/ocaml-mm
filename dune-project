(lang dune 2.0)
(version 0.7.0)
(name mm)
(source (github savonet/ocaml-mm))
(license GPL-2.0)
(authors "Romain Beauxis <toots@rastageeks.org>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name mm)
 (synopsis "The mm library contains high-level to create and manipulate multimedia streams (audio, video, MIDI)")
 (depends
  (dune (>= 2.0))
  dune-configurator)
 (depopts
  alsa
  ao
  (mad (>= 0.5.0))
  pulseaudio
  ocamlsdl
  theora))
