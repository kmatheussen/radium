# mic fixme
# Under windows search for IMG and AMD includes and libraries for EGL and OpenGL ES 1 and OpenGL ES 2

option(VL_GLES_EXAMPLES "Build OpenGL ES 1.x or 2.x examples" ON)

# EGL include
# mic fixme: remove hardcoded path
set(VL_EGL_INCLUDE_DIR "C:/Imagination Technologies/POWERVR SDK/OGLES/Builds/OGLES/Include" CACHE PATH "Path to EGL include directories")
if (VL_EGL_INCLUDE_DIR)
	include_directories(${VL_EGL_INCLUDE_DIR})
endif()
# EGL library
set(VL_EGL_LIB_DIR "C:/Imagination Technologies/POWERVR SDK/OGLES/Builds/OGLES/WindowsX86/Lib" CACHE PATH "Path to directory containing EGL library")
if (VL_EGL_LIB_DIR)
	link_directories(${VL_EGL_LIB_DIR})
endif()
set(VL_EGL_LIBRARY "libEGL.lib" CACHE STRING "EGL library name")

# OpenGL ES library
# mic fixme: remove hardcoded path
#set(VL_GLES_LIB_DIR "NOTFOUND" CACHE PATH "Path to directory containing OpenGL ES library")
if(VL_OPENGL_ES1)
	set(VL_GLES_LIB_DIR "C:/Imagination Technologies/POWERVR SDK/OGLES/Builds/OGLES/WindowsX86/Lib" CACHE PATH "Path to directory containing OpenGL ES library")
elseif(VL_OPENGL_ES2)
	set(VL_GLES_LIB_DIR "C:/Imagination Technologies/POWERVR SDK/OGLES2/Builds/OGLES2/WindowsX86/Lib" CACHE PATH "Path to directory containing OpenGL ES library")
endif()
if (VL_GLES_LIB_DIR)
	link_directories(${VL_GLES_LIB_DIR})
endif()
# mic fixme
# set(VL_GLES_LIBRARY "NOTFOUND" CACHE STRING "OpenGL ES library")
if(VL_OPENGL_ES1)
	set(VL_GLES_LIBRARY "libgles_cm.lib" CACHE STRING "OpenGL ES library")
elseif(VL_OPENGL_ES2)
	set(VL_GLES_LIBRARY "libGLESv2.lib" CACHE STRING "OpenGL ES library")
endif()
