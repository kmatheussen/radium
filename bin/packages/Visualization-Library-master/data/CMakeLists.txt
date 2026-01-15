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
# Data Installation
################################################################################

project(DATA)

option(VL_INSTALL_DATA "Install sample data" OFF)
if(VL_INSTALL_DATA)
	if(VL_UNIX_INSTALL_MODE)
		install(DIRECTORY 3rdparty font glsl images models text volume mol DESTINATION "share/vl/data/")
		install(FILES README.TXT ztest.zip DESTINATION "share/vl/data/")
	else()
		install(DIRECTORY 3rdparty font glsl images models text volume mol DESTINATION "data/")
		install(FILES README.TXT ztest.zip DESTINATION "data/")
	endif()
endif()
