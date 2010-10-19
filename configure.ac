AC_INIT(ocaml-mm, 0.1.0, savonet-users@lists.sourceforge.net)
VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

# Check for OCaml compilers
AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi
if test "$OCAMLOPT" = "no"; then
   BEST="byte"
else
   BEST="byte native"
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install OCaml findlib (the ocamlfind command)])
fi

AC_ARG_ENABLE([debugging],
   AC_HELP_STRING(
      [--enable-debugging],
      [compile with debugging information (backtrace printing in particular)]))

if test "x$enable_debugging" = "xyes" ; then
  OCAMLFLAGS="$OCAMLFLAGS -g"
fi

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I`pwd`/`dirname $0`"

AC_ARG_ENABLE([profiling],
   AC_HELP_STRING(
      [--enable-profiling],
      [compile to generate profiling infomation]))
if test "x$enable_profiling" = "xyes" ; then
  OCAMLNCFLAGS="$OCAMLNCFLAGS -p"
fi

# Check endianess
AC_C_BIGENDIAN(AC_DEFINE([BIGENDIAN], [1], [The target is big endian]),[])

######### OCaml libs ##########

AC_CHECK_OCAML_PKG([alsa])
INCDIRS="$INCDIRS $OCAML_DIR_alsa"
AC_SUBST(OCAML_HAS_alsa)

AC_CHECK_OCAML_PKG([ao])
INCDIRS="$INCDIRS $OCAML_DIR_ao"
AC_SUBST(OCAML_HAS_ao)

AC_CHECK_OCAML_PKG([mad])
INCDIRS="$INCDIRS $OCAML_DIR_mad"
AC_SUBST(OCAML_HAS_mad)

AC_CHECK_OCAML_PKG([ogg])
INCDIRS="$INCDIRS $OCAML_DIR_ogg"
AC_SUBST(OCAML_HAS_ogg)

AC_CHECK_OCAML_PKG([sdl])
INCDIRS="$INCDIRS $OCAML_DIR_sdl"
AC_SUBST(OCAML_HAS_sdl)

AC_CHECK_OCAML_PKG([theora])
INCDIRS="$INCDIRS $OCAML_DIR_theora"
AC_SUBST(OCAML_HAS_theora)

########## OSS ##########

AC_ARG_ENABLE([oss],AC_HELP_STRING([--disable-oss],["don't use OSS"]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([sys/soundcard.h],[W_OSS="yes"],[W_OSS="no"])
fi

AC_SUBST(W_OSS)

########## v4l ##########

AC_ARG_ENABLE([v4l],AC_HELP_STRING([--disable-v4l],["don't use video4linux"]))

if test "x$enable_v4l" != "xno" ; then
  AC_CHECK_HEADERS([linux/videodev.h],[W_V4L="yes"],[W_V4L="no"])
fi

AC_SUBST(W_V4L)




# substitutions to perform
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLFLAGS)
AC_SUBST(OCAMLNCFLAGS)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC)
AC_SUBST(OCAMLMKTOP)	# TODO
AC_SUBST(OCAMLFIND)	# TODO
AC_SUBST(OCAMLFIND_LDCONF)
AC_SUBST(OCAMLCP)	# TODO
AC_SUBST(CAMLLIBPATH)
AC_SUBST(BEST)
AC_SUBST(INCDIRS)
AC_SUBST(CFLAGS)

AC_SUBST(LATEX)		# TODO
AC_SUBST(DVIPS)		# TODO
AC_SUBST(DVIPS)		# TODO
AC_SUBST(PS2PDF)	# TODO

# Finally create the Makefile
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([external/Makefile],[chmod a-w external/Makefile])
AC_CONFIG_FILES([external/Makefile.defs],[chmod a-w external/Makefile.defs])
AC_CONFIG_FILES([src/META])
AC_OUTPUT

cat <<EOMSG


Supported external libraries:
  - Alsa  : $OCAML_HAS_alsa
  - AO    : $OCAML_HAS_ao
  - Mad   : $OCAML_HAS_mad
  - ogg   : $OCAML_HAS_ogg
  - OSS   : $W_OSS
  - SDL   : $OCAML_HAS_sdl
  - Theora: $OCAML_HAS_theora
  - V4L   : $W_V4L

EOMSG
