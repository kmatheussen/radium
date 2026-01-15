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
# VLVolume Library
################################################################################

project(VLVOLUME)

# Gather VLMolecule source files
file(GLOB VLVOLUME_SRC "*.cpp")
file(GLOB VLVOLUME_INC "*.hpp")

add_library(VLVolume ${VL_SHARED_OR_STATIC} ${VLVOLUME_SRC} ${VLVOLUME_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLVolume)

target_link_libraries(VLVolume VLCore VLGraphics)

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLVolume)

# VLVolume headers
install(FILES ${VLVOLUME_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlVolume")
