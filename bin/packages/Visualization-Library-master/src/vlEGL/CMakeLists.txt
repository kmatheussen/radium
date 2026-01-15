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
# VLEGL Library
################################################################################

project(VLEGL)

# Gather VLEGL source files
file(GLOB VLEGL_SRC "*.cpp")
file(GLOB VLEGL_INC "*.hpp")

add_library(VLEGL ${VL_SHARED_OR_STATIC} ${VLEGL_SRC} ${VLEGL_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLEGL)

target_link_libraries(VLEGL VLCore VLGraphics ${VL_EGL_LIBRARY})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLEGL)

# VLEGL headers
install(FILES ${VLEGL_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlEGL")
