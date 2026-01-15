################################################################################
#                                                                              #
#  Copyright (c) 2005-2020, Michele Bosi, Thiago Bastos                        #
#  All rights reserved.                                                        #
#                                                                              #
#  This file is part of Visualization Library                                  #
#  http://visualizationlibrary.org                                             #
#                                                                              #
#  Released under the OSI approved Simplified BSD License                      #
#  http://www.opensource.org/licenses/bsd-license.php                          #
#                                                                              #
################################################################################

################################################################################
# VLQt6 Library
################################################################################

project(VLQt6)

# Gather VLQt6 source files
file(GLOB VLQt6_SRC "*.cpp")
file(GLOB VLQt6_INC "*.hpp")

add_library(VLQt6 ${VL_SHARED_OR_STATIC} ${VLQt6_SRC} ${VLQt6_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLQt6)

target_link_libraries(VLQt6 VLMain Qt6::Widgets Qt6::OpenGLWidgets)

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLQt6)

# VLQt6 headers
install(FILES ${VLQt6_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlQt6")
