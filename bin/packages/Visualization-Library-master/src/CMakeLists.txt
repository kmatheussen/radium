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
# Modules
################################################################################

# VLCore
add_subdirectory("vlCore")

# VLGraphics
add_subdirectory("vlGraphics")

# VLVG
add_subdirectory("vlVG")

# VLMolecule
add_subdirectory("vlMolecule")

# VLVolume
add_subdirectory("vlVolume")

# tools
add_subdirectory("tools")

################################################################################
# GUI Modules (automatically enabled when their requirements are met)
################################################################################

# GLUT
option(VL_GUI_GLUT_SUPPORT "Build GLUT support" OFF)
if(VL_GUI_GLUT_SUPPORT)
	set(GLUT_ROOT_PATH "${3RDPARTY_DIR}")
	find_package(GLUT REQUIRED)
	add_subdirectory("vlGLUT")
endif()

# Qt4
option(VL_GUI_QT4_SUPPORT "Build Qt4 support" OFF)
if(VL_GUI_QT4_SUPPORT)
	find_package(Qt4 COMPONENTS QtCore QtGui QtOpenGL QtMain REQUIRED)
	include("${QT_USE_FILE}")
	add_subdirectory("vlQt4")
endif()


# Qt5
option(VL_GUI_QT5_SUPPORT "Build Qt5 support" OFF)
if(VL_GUI_QT5_SUPPORT)	
        find_package(Qt5OpenGL REQUIRED)
        find_package(Qt5Widgets REQUIRED)	
        include_directories(${Qt5Core_INCLUDE_DIRS})
        include_directories(${Qt5Widgets_INCLUDE_DIRS})        
	add_subdirectory("vlQt5")        
        IF(UNIX)
            IF(CMAKE_COMPILER_IS_GNUCC)
                set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
            ENDIF(CMAKE_COMPILER_IS_GNUCC)
        ENDIF(UNIX)
endif()

if(VL_PLATFORM_WINDOWS)
	option(VL_GUI_WIN32_SUPPORT "Build Win32 support" OFF)
endif()

if(MSVC)
	option(VL_GUI_MFC_SUPPORT "Build MFC support" OFF)
endif()

# EGL bindings are available only under Windows right now
if(VL_PLATFORM_WINDOWS AND (VL_OPENGL_ES1 OR VL_OPENGL_ES2))
	option(VL_GUI_EGL_SUPPORT "Build EGL support" ON)
endif()

if(VL_GUI_MFC_SUPPORT)
  add_subdirectory("vlMFC")
endif()

if(VL_GUI_WIN32_SUPPORT)
	add_subdirectory("vlWin32")
endif()

if(VL_GUI_EGL_SUPPORT)
	add_subdirectory("vlEGL")
endif()

# SDL
option(VL_GUI_SDL_SUPPORT "Build SDL support" OFF)

if(VL_GUI_SDL_SUPPORT)
	find_package(SDL REQUIRED)
	add_subdirectory("vlSDL")
endif()

# wxWidgets
option(VL_GUI_WXWIDGETS_SUPPORT "Build wxWidgets support" OFF)
if(VL_GUI_WXWIDGETS_SUPPORT)
	find_package(wxWidgets COMPONENTS gl core base REQUIRED)
	if (wxWidgets_FOUND)
		add_subdirectory("vlWX")
	endif()
endif()

# Cocoa
#if(APPLE)
#	option(VL_GUI_COCOA_SUPPORT "Build Cocoa support" ON)
#endif()
#if(VL_GUI_COCOA_SUPPORT)
#	add_subdirectory("vlCocoa")
#endif()

################################################################################
# Examples
################################################################################

cmake_dependent_option(VL_GUI_GLUT_EXAMPLES "Build GLUT examples" ON "VL_GUI_GLUT_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_QT4_EXAMPLES "Build Qt4 examples" ON "VL_GUI_QT4_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_QT5_EXAMPLES "Build Qt5 examples" ON "VL_GUI_QT5_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_MFC_EXAMPLES "Build MFC examples" ON "VL_GUI_MFC_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_WIN32_EXAMPLES "Build win32 examples" ON "VL_GUI_WIN32_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_SDL_EXAMPLES "Build SDL examples" ON "VL_GUI_SDL_SUPPORT" OFF)
cmake_dependent_option(VL_GUI_WXWIDGETS_EXAMPLES "Build wxWidgets examples" ON "VL_GUI_WXWIDGETS_SUPPORT" OFF)
cmake_dependent_option(VL_GLES_EXAMPLES "Build OpenGL ES examples" ON "VL_GUI_EGL_SUPPORT" OFF)
#cmake_dependent_option(VL_GUI_COCOA_EXAMPLES "Build Cocoa examples" ON "VL_GUI_COCOA_SUPPORT" OFF)

add_subdirectory("examples")
