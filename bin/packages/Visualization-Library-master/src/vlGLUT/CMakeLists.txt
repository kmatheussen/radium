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
# VLGLUT Library
################################################################################

project(VLGLUT)

# Gather VLGLUT source files
file(GLOB VLGLUT_SRC "*.cpp")
file(GLOB VLGLUT_INC "*.hpp")

include_directories(${GLUT_INCLUDE_DIR})
add_library(VLGLUT ${VL_SHARED_OR_STATIC} ${VLGLUT_SRC} ${VLGLUT_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLGLUT)

target_link_libraries(VLGLUT VLCore VLGraphics ${GLUT_LIBRARIES})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLGLUT)

# VLGLUT headers
install(FILES ${VLGLUT_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlGLUT")
