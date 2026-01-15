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
# TIFF Library
################################################################################

set(TIFF_DIR "${3RDPARTY_DIR}/libtiff")

add_definitions(-D_CRT_SECURE_NO_WARNINGS)

include_directories("${3RDPARTY_DIR}/zlib" "${3RDPARTY_DIR}/jpeg-6b")

#-DOJPEG_SUPPORT not supported
add_definitions(
	-DCCITT_SUPPORT -DPACKBITS_SUPPORT -DLZW_SUPPORT -DTHUNDER_SUPPORT -DNEXT_SUPPORT -DLOGLUV_SUPPORT -DZIP_SUPPORT
	-DPIXARLOG_SUPPORT -DDEFAULT_EXTRASAMPLE_AS_ALPHA -DSTRIPCHOP_DEFAULT=TIFF_STRIPCHOP -DSTRIP_SIZE_DEFAULT=8192
    -DAVOID_WIN32_FILEIO -DJPEG_SUPPORT -DCHECK_JPEG_YCBCR_SUBSAMPLING
)

#if(WIN32)
#	set(PLATFORM_SPECIFIC tif_win32.c)
#endif()
#if(APPLE)
#	set(PLATFORM_SPECIFIC tif_apple.c)
#endif()
#if(UNIX or APPLE)
	set(PLATFORM_SPECIFIC tif_unix.c) # this one works for all platforms
#endif()

if(UNIX AND VL_DYNAMIC_LINKING)
    add_definitions("-fPIC")
endif()

add_library(TIFF STATIC
	"${TIFF_DIR}/${PLATFORM_SPECIFIC}"
	"${TIFF_DIR}/tif_aux.c"
	"${TIFF_DIR}/tif_close.c"
	"${TIFF_DIR}/tif_codec.c"
	"${TIFF_DIR}/tif_color.c"
	"${TIFF_DIR}/tif_compress.c"
	"${TIFF_DIR}/tif_dir.c"
	"${TIFF_DIR}/tif_dirinfo.c"
	"${TIFF_DIR}/tif_dirread.c"
	"${TIFF_DIR}/tif_dirwrite.c"
	"${TIFF_DIR}/tif_dumpmode.c"
	"${TIFF_DIR}/tif_error.c"
	"${TIFF_DIR}/tif_extension.c"
	"${TIFF_DIR}/tif_fax3.c"
	"${TIFF_DIR}/tif_fax3sm.c"
	"${TIFF_DIR}/tif_getimage.c"
	"${TIFF_DIR}/tif_jpeg.c"
	#"${TIFF_DIR}/tif_ojpeg.c"
	"${TIFF_DIR}/tif_flush.c"
	"${TIFF_DIR}/tif_luv.c"
	"${TIFF_DIR}/tif_lzw.c"
	"${TIFF_DIR}/tif_next.c"
	"${TIFF_DIR}/tif_open.c"
	"${TIFF_DIR}/tif_packbits.c"
	"${TIFF_DIR}/tif_pixarlog.c"
	"${TIFF_DIR}/tif_predict.c"
	"${TIFF_DIR}/tif_print.c"
	"${TIFF_DIR}/tif_read.c"
	"${TIFF_DIR}/tif_stream.cxx"
	"${TIFF_DIR}/tif_swab.c"
	"${TIFF_DIR}/tif_strip.c"
	"${TIFF_DIR}/tif_thunder.c"
	"${TIFF_DIR}/tif_tile.c"
	"${TIFF_DIR}/tif_version.c"
	"${TIFF_DIR}/tif_warning.c"
	"${TIFF_DIR}/tif_write.c"
	"${TIFF_DIR}/tif_zip.c"
)

if(NOT VL_DYNAMIC_LINKING)
	VL_INSTALL_TARGET(TIFF)
endif()

VL_PROJECT_ADD(VLCORE INCLUDE_DIRECTORIES "${TIFF_DIR}" LINK_LIBRARIES TIFF)
