AC_INIT([ocaml-mm],[0.3.0],[savonet-users@lists.sourceforge.net])
VERSION=$PACKAGE_VERSION
AC_MSG_RESULT(configuring $PACKAGE_STRING)

AC_BASE_CHECKS()

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I.."

INCDIRS="$INCDIRS .."

AC_CHECK_FUNCS([memalign])

# Check endianess
AC_C_BIGENDIAN(AC_DEFINE([BIGENDIAN], [1], [The target is big endian]),[])

######### OCaml libs ##########

AC_CHECK_OCAML_PKG([alsa])
INCDIRS="$INCDIRS $OCAML_DIR_alsa"
AC_SUBST(OCAML_HAS_alsa)

AC_CHECK_OCAML_PKG([ao])
INCDIRS="$INCDIRS $OCAML_DIR_ao"
AC_SUBST(OCAML_HAS_ao)

AC_CHECK_OCAML_PKG([pulseaudio])
INCDIRS="$INCDIRS $OCAML_DIR_pulseaudio"
AC_SUBST(OCAML_HAS_pulseaudio)

AC_CHECK_OCAML_PKG([gstreamer])
INCDIRS="$INCDIRS $OCAML_DIR_gstreamer"
AC_SUBST(OCAML_HAS_gstreamer)

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

AC_ARG_ENABLE([oss],AS_HELP_STRING([--disable-oss],["don't use OSS"]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([sys/soundcard.h],[W_OSS="yes"],[W_OSS="no"])
fi

AC_SUBST(W_OSS)

########## FFmpeg ##########

AC_ARG_ENABLE([ffmpeg],AS_HELP_STRING([--disable-ffmpeg],["don't use FFmpeg"]))

if test "x$enable_ffmpeg" != "xno" ; then
  AC_CHECK_HEADERS([libavcodec/avcodec.h],[W_FFMPEG="yes"],[W_FFMPEG="no"])
fi

AC_SUBST(W_FFMPEG)

########## v4l ##########

AC_ARG_ENABLE([v4l],AS_HELP_STRING([--disable-v4l],["don't use video4linux"]))

if test "x$enable_v4l" != "xno" ; then
  AC_CHECK_HEADERS([linux/videodev.h],[W_V4L="yes"],[W_V4L="no"])
fi

AC_SUBST(W_V4L)
AC_SUBST(VERSION)
AC_SUBST(INCDIRS)

# Finally create the Makefile
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile],[chmod a-w src/Makefile])
AC_CONFIG_FILES([external/Makefile],[chmod a-w external/Makefile])
AC_CONFIG_FILES([external/Makefile.defs],[chmod a-w external/Makefile.defs])
AC_CONFIG_FILES([src/META])
AC_OUTPUT

cat <<EOMSG


Supported external libraries:
  - Alsa      : $OCAML_HAS_alsa
  - AO        : $OCAML_HAS_ao
  - FFmpeg    : $W_FFMPEG
  - Gstreamer : $OCAML_HAS_gstreamer
  - Mad       : $OCAML_HAS_mad
  - ogg       : $OCAML_HAS_ogg
  - OSS       : $W_OSS
  - Pulseaudio: $OCAML_HAS_pulseaudio
  - SDL       : $OCAML_HAS_sdl
  - Theora    : $OCAML_HAS_theora
  - V4L       : $W_V4L

EOMSG
