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
# ZLib Library - required by VLCore and libtiff
################################################################################

set(ZLIB_DIR "${3RDPARTY_DIR}/zlib")

add_definitions(-D_CRT_SECURE_NO_WARNINGS)

if(UNIX AND VL_DYNAMIC_LINKING)
    add_definitions("-fPIC")
endif()

add_library(ZLib STATIC
	"${ZLIB_DIR}/adler32.c"
	"${ZLIB_DIR}/compress.c"
	"${ZLIB_DIR}/crc32.c"
	"${ZLIB_DIR}/deflate.c"
	"${ZLIB_DIR}/gzio.c"
	"${ZLIB_DIR}/infback.c"
	"${ZLIB_DIR}/inffast.c"
	"${ZLIB_DIR}/inflate.c"
	"${ZLIB_DIR}/inftrees.c"
	"${ZLIB_DIR}/trees.c"
	"${ZLIB_DIR}/uncompr.c"
	"${ZLIB_DIR}/zutil.c"
)

if(NOT VL_DYNAMIC_LINKING)
	VL_INSTALL_TARGET(ZLib)
endif()

VL_PROJECT_ADD(VLCORE INCLUDE_DIRECTORIES "${ZLIB_DIR}" LINK_LIBRARIES ZLib)
