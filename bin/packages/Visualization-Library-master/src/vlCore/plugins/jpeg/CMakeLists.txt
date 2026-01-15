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
# JPEG Library
################################################################################

set(JPG_DIR "${3RDPARTY_DIR}/jpeg-6b")

add_definitions(-D_CRT_SECURE_NO_WARNINGS)

if(UNIX AND VL_DYNAMIC_LINKING)
    add_definitions("-fPIC")
endif()

add_library(JPG STATIC
	"${JPG_DIR}/jmemnobs.c"
	"${JPG_DIR}/jcomapi.c"
	"${JPG_DIR}/jutils.c"
	"${JPG_DIR}/jerror.c"
	"${JPG_DIR}/jmemmgr.c"
	"${JPG_DIR}/jcapimin.c"
	"${JPG_DIR}/jcapistd.c"
	"${JPG_DIR}/jctrans.c"
	"${JPG_DIR}/jcparam.c"
	"${JPG_DIR}/jdatadst.c"
	"${JPG_DIR}/jcinit.c"
	"${JPG_DIR}/jcmaster.c"
	"${JPG_DIR}/jcmarker.c"
	"${JPG_DIR}/jcmainct.c"
	"${JPG_DIR}/jcprepct.c"
	"${JPG_DIR}/jccoefct.c"
	"${JPG_DIR}/jccolor.c"
	"${JPG_DIR}/jcsample.c"
	"${JPG_DIR}/jchuff.c"
	"${JPG_DIR}/jcphuff.c"
	"${JPG_DIR}/jcdctmgr.c"
	"${JPG_DIR}/jfdctfst.c"
	"${JPG_DIR}/jfdctflt.c"
	"${JPG_DIR}/jfdctint.c"
	"${JPG_DIR}/jdapimin.c"
	"${JPG_DIR}/jdapistd.c"
	"${JPG_DIR}/jdtrans.c"
	"${JPG_DIR}/jdatasrc.c"
	"${JPG_DIR}/jdmaster.c"
	"${JPG_DIR}/jdinput.c"
	"${JPG_DIR}/jdmarker.c"
	"${JPG_DIR}/jdhuff.c"
	"${JPG_DIR}/jdphuff.c"
	"${JPG_DIR}/jdmainct.c"
	"${JPG_DIR}/jdcoefct.c"
	"${JPG_DIR}/jdpostct.c"
	"${JPG_DIR}/jddctmgr.c"
	"${JPG_DIR}/jidctfst.c"
	"${JPG_DIR}/jidctflt.c"
	"${JPG_DIR}/jidctint.c"
	"${JPG_DIR}/jidctred.c"
	"${JPG_DIR}/jdsample.c"
	"${JPG_DIR}/jdcolor.c"
	"${JPG_DIR}/jquant1.c"
	"${JPG_DIR}/jquant2.c"
	"${JPG_DIR}/jdmerge.c"
)

if(NOT VL_DYNAMIC_LINKING)
	VL_INSTALL_TARGET(JPG)
endif()

VL_PROJECT_ADD(VLCORE INCLUDE_DIRECTORIES "${JPG_DIR}" LINK_LIBRARIES JPG)
