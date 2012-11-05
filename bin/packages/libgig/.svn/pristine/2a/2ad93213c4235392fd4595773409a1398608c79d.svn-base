SUBDIRS = testcases

# to prevent compile errors on some systems
AM_CXXFLAGS = -pedantic -Wreturn-type

# link gigextract either against libaudiofile or libsndfile
# (if both exist on the system, prefer libsndfile)
if HAVE_SNDFILE
gigextract_libs = $(SNDFILE_LIBS)
gigextract_flags = $(SNDFILE_CFLAGS)
else
gigextract_libs = $(AUDIOFILE_LIBS) -ldl
gigextract_flags = $(AUDIOFILE_CFLAGS)
endif

libgigincludedir = $(includedir)
libgiginclude_HEADERS = RIFF.h DLS.h SF.h gig.h

lib_LTLIBRARIES = libgig.la
libgig_la_SOURCES = RIFF.cpp DLS.cpp SF.cpp gig.cpp
libgig_la_LDFLAGS = -no-undefined -version-info @SHARED_VERSION_INFO@ @SHLIB_VERSION_ARG@
if WIN32
libgig_la_LIBADD = -lrpcrt4
endif
if MAC
libgig_la_LDFLAGS += -framework CoreFoundation
endif
bin_PROGRAMS = rifftree dlsdump gigdump gigextract sf2dump
rifftree_SOURCES = rifftree.cpp
rifftree_LDADD = libgig.la
dlsdump_SOURCES = dlsdump.cpp
dlsdump_LDADD = libgig.la
gigdump_SOURCES = gigdump.cpp
gigdump_LDADD = libgig.la
gigextract_SOURCES = gigextract.cpp
gigextract_LDADD = libgig.la $(gigextract_libs)
gigextract_CFLAGS = $(gigextract_flags)
gigextract_CXXFLAGS = $(gigextract_flags)
sf2dump_SOURCES = sf2dump.cpp
sf2dump_LDADD = libgig.la

EXTRA_DIST = helper.h
