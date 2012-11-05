#------------------------------------------------------------------------------------
# Library's "official" release version:

m4_define(libgig_release_major, 3)
m4_define(libgig_release_minor, 3)
m4_define(libgig_release_build, 0.svn3)


AC_INIT([libgig],[libgig_release_major.libgig_release_minor.libgig_release_build])
AC_CONFIG_SRCDIR([configure.ac])

#------------------------------------------------------------------------------------
# The following is the libtool / shared library version. This doesn't have to
# do anything with the release version. It MUST conform to the following rules:
#
#  1. Start with version information of `0:0:0' for each libtool library.
#  2. Update the version information only immediately before a public release of
#     your software. More frequent updates are unnecessary, and only guarantee
#     that the current interface number gets larger faster.
#  3. If the library source code has changed at all since the last update, then
#     increment revision (`c:r:a' becomes `c:r+1:a').
#  4. If any interfaces have been added, removed, or changed since the last update,
#     increment current, and set revision to 0.
#  5. If any interfaces have been added since the last public release, then increment
#     age.
#  6. If any interfaces have been removed since the last public release, then set age
#     to 0.

LIBGIG_LT_CURRENT=10
LIBGIG_LT_REVISION=0
LIBGIG_LT_AGE=4
SHARED_VERSION_INFO="$LIBGIG_LT_CURRENT:$LIBGIG_LT_REVISION:$LIBGIG_LT_AGE"


AC_LANG([C++])
AC_PROG_CXX
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

AC_C_BIGENDIAN

AC_SUBST(SHLIB_VERSION_ARG)
AC_SUBST(SHARED_VERSION_INFO)
AC_SUBST(LIBGIG_RELEASE_MAJOR, libgig_release_major)
AC_SUBST(LIBGIG_RELEASE_MINOR, libgig_release_minor)
AC_SUBST(LIBGIG_RELEASE_BUILD, libgig_release_build)

have_audio_file_lib="false"

# check for presence of libsndfile
PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.2, ac_cv_sndfile=1, ac_cv_sndfile=0)
AM_CONDITIONAL(HAVE_SNDFILE, test $ac_cv_sndfile = "1")
AC_DEFINE_UNQUOTED([HAVE_SNDFILE],${ac_cv_sndfile}, [Set to 1 if you have libsndfile.])
AC_SUBST(SNDFILE_CFLAGS)
AC_SUBST(SNDFILE_LIBS)
if test "$ac_cv_sndfile" = "1"; then
    have_audio_file_lib="true";
else
    # no libsndfile? then check for presence of libaudiofile
    PKG_CHECK_MODULES(AUDIOFILE, audiofile >= 0.2.3, ac_cv_audiofile=1, ac_cv_audiofile=0)
    AC_DEFINE_UNQUOTED([HAVE_AUDIOFILE],${ac_cv_audiofile}, [Set to 1 if you have libaudiofile.])
    AC_SUBST(AUDIOFILE_CFLAGS)
    AC_SUBST(AUDIOFILE_LIBS)
    if test "$ac_cv_audiofile" = "1"; then
        have_audio_file_lib="true";
    fi
fi
if test "$have_audio_file_lib" = "false"; then
    echo "No audio file library found!"
    echo "Sorry, you need either libsndfile or libaudiofile to compile libgig."
    echo "This is needed for the gigextract utility to write audio files."
    exit -1;
fi

AC_CHECK_HEADERS(uuid/uuid.h)
AC_SEARCH_LIBS(uuid_generate, uuid)
AC_CHECK_FUNCS(uuid_generate)

case "$host" in
    *mingw*|*cygwin*)
        win32=yes
        ;;
    *)
        win32=no
        ;;
esac
AM_CONDITIONAL(WIN32, test "$win32" = "yes")

case "$host" in
    *-*-darwin*)
        mac=yes
        ;;
    *)
        mac=no
        ;;
esac
AM_CONDITIONAL(MAC, test "$mac" = "yes")

if test "$ac_cv_func_uuid_generate" = no -a "$mac" = no -a "$win32" = no; then
    AC_MSG_WARN([No UUID generate function found.
*** libgig will not be able to create DLSIDs in DLS and gig files.
*** Install libuuid from e2fsprogs to solve this. Package might be called
*** uuid-dev, libext2fs2-devel or e2fsprogs-devel.])
fi

AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE

AC_CONFIG_FILES([\
    Doxyfile \
    gig.pc \
    libgig.spec \
    Makefile \
    src/Makefile \
    src/testcases/Makefile \
    doc/Makefile \
    man/Makefile \
    man/dlsdump.1 \
    man/gigdump.1 \
    man/gigextract.1 \
    man/rifftree.1 \
    debian/Makefile \
    osx/Makefile \
    osx/libgig.xcodeproj/Makefile \
    win32/Makefile \
    win32/libgig.dev \
])
AC_OUTPUT
