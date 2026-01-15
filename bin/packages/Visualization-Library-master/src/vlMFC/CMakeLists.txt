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
# VLMFC Library
################################################################################

project(VLMFC)

# Gather VLMFC source files
file(GLOB VLMFC_SRC "*.cpp")
file(GLOB VLMFC_INC "*.hpp")

add_definitions(-D_AFXDLL)
add_library(VLMFC ${VL_SHARED_OR_STATIC} ${VLMFC_SRC} ${VLMFC_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLMFC)

target_link_libraries(VLMFC VLCore VLGraphics VLWin32 ${wxWidgets_LIBRARIES})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLMFC)

# VLMolecule headers
install(FILES ${VLMFC_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlMFC")
