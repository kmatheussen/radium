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

# Current Version
set(VLVERSION_MAJOR "2011")
set(VLVERSION_MINOR "9")
set(VLVERSION_PATCH "1160")
set(VLVERSION "${VLVERSION_MAJOR}.${VLVERSION_MINOR}")

################################################################################
# Initialization
################################################################################

project(Visualization_Library_SDK-${VLVERSION_MAJOR}.${VLVERSION_MINOR})

# Must be called after project!
cmake_minimum_required(VERSION 2.6)

################################################################################
# Global Build Settings
################################################################################

# Platform detect, used to configure platform.hpp.in
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(VL_PLATFORM_MACOSX 1)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	set(VL_PLATFORM_LINUX 1)
	option(VL_UNIX_INSTALL_MODE "Set to ON to install VL into default UNIX path structure." OFF)
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Windows")
	set(VL_PLATFORM_WINDOWS 1) 
else()
	message(FATAL_ERROR "Unable to detect platform!")
endif()
message(STATUS "System detected: \"${CMAKE_SYSTEM_NAME}\"")

# OpenGL, OpenGL ES 1 or OpenGL ES 2 mode
set(VL_OPENGL_MODE "OPENGL" CACHE STRING "Set it to OPENGL, OPENGL_ES1 or OPENGL_ES2 to build VL for OpenGL, OpenGL ES 1.x or OpenGL ES 2.x")
# Setup variables used to configure platform.hpp.in
if( VL_OPENGL_MODE STREQUAL  "OPENGL")
	set(VL_OPENGL 1)
	message(STATUS "Configuring for OpenGL 1.x/2.x/3.x/4.x")
elseif( VL_OPENGL_MODE STREQUAL  "OPENGL_ES1")
	set(VL_OPENGL_ES1 1)
	message(STATUS "Configuring for OpenGL ES 1.x")
elseif( VL_OPENGL_MODE STREQUAL  "OPENGL_ES2")
	set(VL_OPENGL_ES2 1)
	message(STATUS "Configuring for OpenGL ES 2.x")
else()
	message(FATAL_ERROR "Invalid VL_OPENGL_MODE! Valid modes are: OPENGL, OPENGL_ES1, OPENGL_ES2.")
endif()

# Dynamic vs Static Linking
option(VL_DYNAMIC_LINKING "Set to ON to build VL for dynamic linking, or OFF for static." ON)
if(VL_DYNAMIC_LINKING)
	add_definitions(-DVL_DYNAMIC_LINKING)
	set(VL_SHARED_OR_STATIC "SHARED")
else()
	add_definitions(-DVL_STATIC_LINKING)
	set(VL_SHARED_OR_STATIC "STATIC")
endif()

# Common Dirs
set(VL_DATA_DIR "${CMAKE_SOURCE_DIR}/data")
set(3RDPARTY_DIR "${CMAKE_SOURCE_DIR}/src/3rdparty")

# Header Install Dir
if(VL_UNIX_INSTALL_MODE)
	set(VL_INCLUDE_INSTALL_DIR "include/vl")
else()
	set(VL_INCLUDE_INSTALL_DIR "include")
endif()

# Add our dir to the CMake modules path and include our InternalMacros file
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" )
include( InternalMacros )

# Debug postfix for all libraries.
set(CMAKE_DEBUG_POSTFIX "-d")

# Default output locations for the various target types.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "lib")

# Shared include paths for all subprojects
include_directories( "src" "${CMAKE_BINARY_DIR}/src" "${3RDPARTY_DIR}/Khronos" )

# High Warning Level
if(MSVC10)
	set(CMAKE_CXX_FLAGS "/W4 /EHsc /MP")
	add_definitions(-D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES)
elseif(MSVC)
	set(CMAKE_CXX_FLAGS "/W4 /EHsc")
	add_definitions(-D_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES)
else()
	set(CMAKE_CXX_FLAGS "-W -Wall") # see also: -W -Wall -Wwrite-strings -Wcast-qual -Wconversion -Wshadow
endif()

if(WIN32)
	add_definitions(-DUNICODE)
endif()

if(MSVC)
	set(WINVER "0x0600" CACHE STRING "WINVER version (see MSDN documentation)")
	add_definitions(-DWINVER=${WINVER})
	add_definitions(-D_WIN32_WINNT=${WINVER})
endif()

# Required Dependencies

if( VL_OPENGL_MODE STREQUAL "OPENGL")
	find_package(OpenGL REQUIRED)
	set(VL_OPENGL_LIBRARIES ${OPENGL_LIBRARIES})
endif()

if( VL_OPENGL_ES1 OR VL_OPENGL_ES2 )
	include( SetupGLES )
	set(VL_OPENGL_LIBRARIES ${VL_GLES_LIBRARY} ${VL_EGL_LIBRARY})
endif()

################################################################################
# Packaging
################################################################################

set(CPACK_PACKAGE_NAME					"${CMAKE_PROJECT_NAME}")
set(CPACK_PACKAGE_VERSION_MAJOR			"${VLVERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR			"${VLVERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH			"${VLVERSION_PATCH}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY	"A lightweight C++ OpenGL middleware for 2D and 3D graphics.")

if(WIN32)
	set(CPACK_GENERATOR "ZIP")
else()
	set(CPACK_GENERATOR "TBZ2")
endif()

include( CPack )

################################################################################
# Subdirectories
################################################################################

add_subdirectory("docs")
add_subdirectory("data")
add_subdirectory("src")

################################################################################
# Install Rules
################################################################################

if(VL_UNIX_INSTALL_MODE)
	install(FILES "LICENSE.TXT" "README.md" DESTINATION "share/vl")
	install(FILES "cmake/FindVL.cmake" DESTINATION "share/cmake/Modules")
else()
	install(FILES "LICENSE.TXT" "README.md" DESTINATION ".")
	install(FILES "cmake/FindVL.cmake" DESTINATION "cmake")
endif()
set(CMAKE_CXX_FLAGS "$MYFLAGS")
