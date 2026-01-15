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
# VLSDL Library
################################################################################

project(VLSDL)

# Gather VLSDL source files
file(GLOB VLSDL_SRC "*.cpp")
file(GLOB VLSDL_INC "*.hpp")

include_directories(${SDL_INCLUDE_DIR})

add_library(VLSDL ${VL_SHARED_OR_STATIC} ${VLSDL_SRC} ${VLSDL_INC})
VL_DEFAULT_TARGET_PROPERTIES(VLSDL)

target_link_libraries(VLSDL VLCore VLGraphics ${SDL_LIBRARY})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLSDL)

# VLSDL headers
install(FILES ${VLSDL_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlSDL")
