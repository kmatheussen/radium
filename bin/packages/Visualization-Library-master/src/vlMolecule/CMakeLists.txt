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
# VLMolecule Library
################################################################################

project(VLMOLECULE)

# Gather VLMolecule source files
file(GLOB VLMOLECULE_SRC "*.cpp")
file(GLOB VLMOLECULE_INC "*.hpp")

add_library(VLMolecule ${VL_SHARED_OR_STATIC} ${VLMOLECULE_SRC} ${VLMOLECULE_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLMolecule)

target_link_libraries(VLMolecule VLCore VLGraphics ${OPENGL_glu_LIBRARY})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLMolecule)

# VLMolecule headers
install(FILES ${VLMOLECULE_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlMolecule")
