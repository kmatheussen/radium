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

project(TOOLS)

################################################################################
# Build the tools executables
################################################################################

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Common libraries
set(VL_LIBS_BASE VLGraphics VLCore)

# vlxtool
add_executable(vlxtool vlxtool.cpp)
target_link_libraries(vlxtool ${VL_LIBS_BASE})
VL_INSTALL_TARGET(vlxtool)
