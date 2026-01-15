################################################################################
#                                                                              #
#  Copyright (c) 2005-2011, Michele Bosi, Thiago Bastos                        #
#  All rights reserved.                                                        #
#                                                                              #
#  This file is part of Visualization Library                                  #
#  http://www.visualizationlibrary.org                                         #
#                                                                              #
#  Released under the OSI approved Simplified BSD License                      #
#  http://www.opensource.org/licenses/bsd-license.php                          #
#                                                                              #
################################################################################

################################################################################
# FreeType Library
################################################################################

set(FT2_DIR "${3RDPARTY_DIR}/freetype")
set(FT2_SRC "${FT2_DIR}/src")

include_directories("${FT2_DIR}/include")

add_definitions(-D_LIB -DFT2_BUILD_LIBRARY -D_CRT_SECURE_NO_WARNINGS)

# Notes:
# 'ftcache.c'  needs 'ftglyph.c'
# 'ftfstype.c' needs 'fttype1.c'
# 'ftglyph.c'  needs 'ftbitmap.c'
# 'ftstroke.c' needs 'ftglyph.c'
# 'ftsynth.c'  needs 'ftbitmap.c'
# 'cff.c'      needs 'sfnt.c', 'pshinter.c', and 'psnames.c'
# 'truetype.c' needs 'sfnt.c' and 'psnames.c'
# 'type1.c'    needs 'psaux.c' 'pshinter.c', and 'psnames.c'
# 'type1cid.c' needs 'psaux.c', 'pshinter.c', and 'psnames.c'
# 'type42.c'   needs 'truetype.c'

set(FT2_SOURCES
	# -- base components (required)

	"${FT2_SRC}/base/ftsystem.c"
	"${FT2_SRC}/base/ftinit.c"
	"${FT2_SRC}/base/ftdebug.c"

	"${FT2_SRC}/base/ftbase.c"

	"${FT2_SRC}/base/ftbbox.c"       # -- recommended, see <freetype/ftbbox.h>
	"${FT2_SRC}/base/ftglyph.c"      # -- recommended, see <freetype/ftglyph.h>

	"${FT2_SRC}/base/ftbdf.c"        # -- optional, see <freetype/ftbdf.h>
	"${FT2_SRC}/base/ftbitmap.c"     # -- optional, see <freetype/ftbitmap.h>
	"${FT2_SRC}/base/ftcid.c"        # -- optional, see <freetype/ftcid.h>
	"${FT2_SRC}/base/ftfstype.c"     # -- optional
	"${FT2_SRC}/base/ftgasp.c"       # -- optional, see <freetype/ftgasp.h>
	"${FT2_SRC}/base/ftgxval.c"      # -- optional, see <freetype/ftgxval.h>
	"${FT2_SRC}/base/ftlcdfil.c"     # -- optional, see <freetype/ftlcdfil.h>
	"${FT2_SRC}/base/ftmm.c"         # -- optional, see <freetype/ftmm.h>
	"${FT2_SRC}/base/ftotval.c"      # -- optional, see <freetype/ftotval.h>
	"${FT2_SRC}/base/ftpatent.c"     # -- optional
	"${FT2_SRC}/base/ftpfr.c"        # -- optional, see <freetype/ftpfr.h>
	"${FT2_SRC}/base/ftstroke.c"     # -- optional, see <freetype/ftstroke.h>
	"${FT2_SRC}/base/ftsynth.c"      # -- optional, see <freetype/ftsynth.h>
	"${FT2_SRC}/base/fttype1.c"      # -- optional, see <freetype/t1tables.h>
	"${FT2_SRC}/base/ftwinfnt.c"     # -- optional, see <freetype/ftwinfnt.h>
	"${FT2_SRC}/base/ftxf86.c"       # -- optional, see <freetype/ftxf86.h>
	#"${FT2_SRC}/base/ftmac.c"       # -- only on the Macintosh

	# -- font drivers (optional; at least one is needed)

	"${FT2_SRC}/bdf/bdf.c"           # -- BDF font driver
	"${FT2_SRC}/cff/cff.c"           # -- CFF/OpenType font driver
	"${FT2_SRC}/cid/type1cid.c"      # -- Type 1 CID-keyed font driver
	"${FT2_SRC}/pcf/pcf.c"           # -- PCF font driver
	"${FT2_SRC}/pfr/pfr.c"           # -- PFR/TrueDoc font driver
	"${FT2_SRC}/sfnt/sfnt.c"         # -- SFNT files support
	# (TrueType & OpenType)
	"${FT2_SRC}/truetype/truetype.c" # -- TrueType font driver
	"${FT2_SRC}/type1/type1.c"       # -- Type 1 font driver
	"${FT2_SRC}/type42/type42.c"     # -- Type 42 font driver
	"${FT2_SRC}/winfonts/winfnt.c"   # -- Windows FONT / FNT font driver

	# -- rasterizers (optional; at least one is needed for vector formats)

	"${FT2_SRC}/raster/raster.c"     # -- monochrome rasterizer
	"${FT2_SRC}/smooth/smooth.c"     # -- anti-aliasing rasterizer

	# -- auxiliary modules (optional)

	"${FT2_SRC}/autofit/autofit.c"   # -- auto hinting module
	"${FT2_SRC}/cache/ftcache.c"     # -- cache sub-system (in beta)
	"${FT2_SRC}/gzip/ftgzip.c"       # -- support for compressed fonts (.gz)
	"${FT2_SRC}/lzw/ftlzw.c"         # -- support for compressed fonts (.Z)
	"${FT2_SRC}/gxvalid/gxvalid.c"   # -- TrueTypeGX/AAT table validation
	"${FT2_SRC}/otvalid/otvalid.c"   # -- OpenType table validation
	"${FT2_SRC}/psaux/psaux.c"       # -- PostScript Type 1 parsing
	"${FT2_SRC}/pshinter/pshinter.c" # -- PS hinting module
	"${FT2_SRC}/psnames/psnames.c"   # -- PostScript glyph names support
)

if(APPLE)
	list(APPEND FT2_SOURCES "${FT2_SRC}/base/ftmac.c")
	find_library(APP_KIT AppKit)
	mark_as_advanced(APP_KIT)
	VL_PROJECT_ADD(VLGRAPHICS LINK_LIBRARIES ${APP_KIT})
endif()

if(UNIX AND VL_DYNAMIC_LINKING)
    add_definitions("-fPIC")
endif()

add_library(FreeType STATIC ${FT2_SOURCES})

if(NOT VL_DYNAMIC_LINKING)
	VL_INSTALL_TARGET(FreeType)
endif()

VL_PROJECT_ADD(VLGRAPHICS INCLUDE_DIRECTORIES "${FT2_DIR}/include" LINK_LIBRARIES FreeType)
