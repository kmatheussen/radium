################################################################################
#                                                                              #
#  Copyright (c) 2005-2011, Michele Bosi                                       #
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
# COLLADA DOM Library
################################################################################

# See also http://www.collada.org/mediawiki/index.php/DOM_guide:_Setting_up

# use find file to double check that the files are there where they are supposed to be
if (VL_IO_3D_COLLADA)

	set(COLLADA_DOM_INCLUDE_DIRECTORIES "NOTFOUND" CACHE STRING "COLLADA DOM 2.2 include directories separated by ';', like: <DOM_DIR>/include;<DOM_DIR>/include/1.4")
	set(COLLADA_DOM_LIBRARY_DEBUG "NOTFOUND" CACHE STRING "COLLADA DOM 2.2 dll library (debug mode)")
	set(COLLADA_DOM_LIBRARY_RELEASE "NOTFOUND" CACHE STRING"COLLADA DOM 2.2 dll library (release mode)")

	# Examples
	# D:/VL/COLLADA/collada-dom/dom/include;D:/VL/COLLADA/collada-dom/dom/include/1.4
	# D:/VL/COLLADA/collada-dom/dom/build/vc10-1.4-d/libcollada14dom22-d.lib
	# D:/VL/COLLADA/collada-dom/dom/build/vc10-1.4/libcollada14dom22.lib

	if (NOT EXISTS ${COLLADA_DOM_LIBRARY_DEBUG})
		message( FATAL_ERROR "Please specify a valid COLLADA_DOM_LIBRARY_DEBUG!")
	endif()

	if (NOT EXISTS ${COLLADA_DOM_LIBRARY_RELEASE})
		message( FATAL_ERROR "Please specify a valid COLLADA_DOM_LIBRARY_RELEASE!")
	endif()

	VL_PROJECT_ADD(VLGRAPHICS 
		DEFINITIONS "-DDOM_INCLUDE_LIBXML" "-DPCRE_STATIC" "-DBOOST_ALL_NO_LIB" "-DNO_BOOST"
		INCLUDE_DIRECTORIES ${COLLADA_DOM_INCLUDE_DIRECTORIES}
		LINK_LIBRARIES_DEBUG ${COLLADA_DOM_LIBRARY_DEBUG}
		LINK_LIBRARIES_RELEASE ${COLLADA_DOM_LIBRARY_RELEASE}
	)

endif()
