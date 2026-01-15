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
# VLWX Library
################################################################################

project(VLWX)

include(${wxWidgets_USE_FILE})

# Gather VLWX source files
file(GLOB VLWX_SRC "*.cpp")
file(GLOB VLWX_INC "*.hpp")

add_library(VLWX ${VL_SHARED_OR_STATIC} ${VLWX_SRC} ${VLWX_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLWX)

target_link_libraries(VLWX VLCore VLGraphics  ${wxWidgets_LIBRARIES})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLWX)

# VLWX headers
install(FILES ${VLWX_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlWX")
