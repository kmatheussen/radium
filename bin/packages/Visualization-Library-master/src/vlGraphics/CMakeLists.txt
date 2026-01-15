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
# VLGraphics Library
################################################################################

project(VLGRAPHICS)

# Handle compile-time plugins for VLGraphics
add_subdirectory("plugins")

# Gather VLGraphics source files
file(GLOB VLGRAPHICS_SRC "*.cpp")
file(GLOB VLGRAPHICS_INC "*.hpp")
file(GLOB VLGRAPHICS_GL_INC "GL/*.hpp")

if( NOT VL_OPENGL )
	list(REMOVE_ITEM VLGRAPHICS_SRC "${VLGRAPHICS_SOURCE_DIR}/Tessellator.cpp")
	list(REMOVE_ITEM VLGRAPHICS_INC "${VLGRAPHICS_SOURCE_DIR}/Tessellator.hpp")

	list(REMOVE_ITEM VLGRAPHICS_SRC "${VLGRAPHICS_SOURCE_DIR}/Extrusion.cpp")
	list(REMOVE_ITEM VLGRAPHICS_INC "${VLGRAPHICS_SOURCE_DIR}/Extrusion.hpp")
	message(STATUS "Removing Tessellator class, not supported in OpenGL ES 1.x and 2.x (requires GLU)")
endif()

# Handle extras added by plugins
VL_PROJECT_GET(VLGRAPHICS _SOURCES _DEFINITIONS _INCLUDE_DIRS _EXTRA_LIBS_D _EXTRA_LIBS_R)

add_definitions(${_DEFINITIONS})
include_directories(${_INCLUDE_DIRS})

# Khronos OpenGL headers
set(KHRONOS_DIR "${3RDPARTY_DIR}/Khronos")
set(KHRONOS_INC ${KHRONOS_DIR}/GL/mesa_gl.h
				${KHRONOS_DIR}/GL/khronos_glext.h 
				${KHRONOS_DIR}/GL/khronos_wglext.h 				
				${KHRONOS_DIR}/GLES/gles_extra_defines.h
				${KHRONOS_DIR}/GLES/khronos_gl.h
				${KHRONOS_DIR}/GLES/khronos_glext.h
				${KHRONOS_DIR}/GLES/khronos_glplatform.h
				${KHRONOS_DIR}/GLES2/gles_extra_defines.h
				${KHRONOS_DIR}/GLES2/khronos_gl2.h
				${KHRONOS_DIR}/GLES2/khronos_gl2ext.h
				${KHRONOS_DIR}/GLES2/khronos_gl2platform.h)

add_library(VLGraphics ${VL_SHARED_OR_STATIC} ${VLGRAPHICS_SRC} ${VLGRAPHICS_INC} ${KHRONOS_INC} ${_SOURCES})
VL_DEFAULT_TARGET_PROPERTIES(VLGraphics)

target_link_libraries(VLGraphics VLCore ${VL_OPENGL_LIBRARIES})

foreach(libName ${_EXTRA_LIBS_D})
	target_link_libraries(VLGraphics debug ${libName})
endforeach()

foreach(libName ${_EXTRA_LIBS_R})
	target_link_libraries(VLGraphics optimized ${libName})
endforeach()

################################################################################
# Source Groups
################################################################################

source_group("plugins" FILES ${_SOURCES})

################################################################################
# Install Rules
################################################################################

VL_INSTALL_TARGET(VLGraphics)

# VLGraphics headers
install(FILES ${VLGRAPHICS_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlGraphics")
install(FILES ${VLGRAPHICS_GL_INC} DESTINATION "${VL_INCLUDE_INSTALL_DIR}/vlGraphics/GL")

# Khronos headers
install(DIRECTORY "${KHRONOS_DIR}/GL" DESTINATION ${VL_INCLUDE_INSTALL_DIR})
