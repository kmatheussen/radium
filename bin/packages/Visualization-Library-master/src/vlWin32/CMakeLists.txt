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
# VLWin32 Library
################################################################################

project(VLWin32)

# Gather VLWin32 source files
file(GLOB VLWin32_SRC "*.cpp")
file(GLOB VLWin32_INC "*.hpp")

add_library(VLWin32 ${VL_SHARED_OR_STATIC} ${VLWin32_SRC} ${VLWin32_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLWin32)

target_link_libraries(VLWin32 VLCore VLGraphics)

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLWin32)

# VLWin32 headers
install(FILES ${VLWin32_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlWin32")
