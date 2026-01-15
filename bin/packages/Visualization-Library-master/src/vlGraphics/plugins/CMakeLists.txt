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
# Compile-time Plugins for VLGraphics
################################################################################

# VLGraphics requires FreeType (not really a plugin, it's here for convenience)


# List of "3D IO" plugins
set(VLGRAPHICS_PLUGINS "VLX" "3DS" "AC3D" "MD2" "OBJ" "PLY" "STL")
set(INSTALL_DIR "${VL_INCLUDE_INSTALL_DIR}/vlGraphics/plugins")

# Process plugins
VL_PROCESS_PROJECT_PLUGINS(VLGRAPHICS VL_IO_3D_ ${INSTALL_DIR} ${VLGRAPHICS_PLUGINS})

# COLLADA plugin managed separately
option(VL_IO_3D_COLLADA "Enable COLLADA support (requires COLLADA DOM)" OFF)
if(VL_IO_3D_COLLADA)
	file(GLOB COLLADA_SRC "COLLADA/*.cpp")
	file(GLOB COLLADA_INC "COLLADA/*.hpp")
	VL_PROJECT_ADD(VLGRAPHICS DEFINITIONS "-DVL_IO_3D_COLLADA" SOURCES ${COLLADA_SRC} ${COLLADA_INC} )
	install(FILES ${COLLADA_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlGraphics/plugins/COLLADA")
endif()

################################################################################
# Add external/3rd-party dependencies as necessary
################################################################################

if(VL_IO_3D_COLLADA)
	add_subdirectory("collada_dom")
endif()
