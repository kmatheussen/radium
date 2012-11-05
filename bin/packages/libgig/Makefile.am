# require automake >= 1.5 (mandatory for option dist-bzip2)
AUTOMAKE_OPTIONS = 1.5 dist-bzip2

SUBDIRS = doc man src debian win32 osx

# to prevent compile errors on some systems
AM_CXXFLAGS = -pedantic

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = gig.pc

EXTRA_DIST = libgig.kdevelop

docs: Doxyfile
	@echo '*** Running doxygen ***'
	doxygen Doxyfile
