#	Find the Visualization Library.
#
#	Sample usage:
#		find_package(VL COMPONENTS VLCore VLGraphics VLVolume REQUIRED)
#		include_directories(${VL_INCLUDE_DIRS})
#		target_link_libraries(myexe ${VL_LIBRARIES})
#
#	The following variables are accepted as input:
#		VL_ROOT (as a CMake or environment variable)
#			- Directory where VL was installed (the "install prefix").
#
#	The following variables are defined by the module:
#		VL_FOUND - Whether VL was found.
#		VL_INCLUDE_DIRS - Directories for including the VL module headers.
#		VL_LIBRARIES - Path to all the requested VL libraries.
#
#	This module also defines variables for each requested VL component:
#		VL_<component-name>_LIBRARIES - The component's libraries (or empty
#			if the component is not available).
#
#   MODULES:
#
#	Visualization Library modules:
#		VLCore:     core functionalities.
#		VLGraphics: base graphics functionalities.
#		VLVolume:   volume rendering.
#		VLMolecule: molecule rendering.
#		VLVG:       vector graphics.
#        
#   Modules required if VL was compiled as a static library:
#       FreeType: FreeType library as compiled by VL.
#       JPG:      JPG library as compiled by VL.
#       PNG:      PNG library as compiled by VL.
#       TIFF:     TIFF library as compiled by VL.
#       ZLib:     zlib library as compiled by VL.
#
#       In this case remember to call 'add_definitions(-DVL_STATIC_LINKING)'
#       before your library or executable.
#
#   GUI binding modules:
#       VLWin32: Win32 gui bindings.
#       VLQt4:   Qt4 gui bindings.
#       VLQt5:   Qt5 gui bindings.
#       VLMFC:   MFC gui bindings.
#       VLWX:    wxWidgets gui bindings.
#       VLGLUT:  GLUT gui bindings.
#       VLSDL:   SDL gui bindings.
#       VLEGL:   EGL support [EXPERIMENTAL]

macro(_vl_find_library _var)
	find_library( ${_var}
		NAMES ${ARGN}
		PATHS ${VL_ROOT} ENV VL_ROOT
		PATH_SUFFIXES "lib"
	)
endmacro()

macro(_vl_find_component _name)
	_vl_find_library(VL_${_name}_LIBRARY ${_name})
	_vl_find_library(VL_${_name}_LIBRARY_DEBUG ${_name}-d)

	if(VL_${_name}_LIBRARY AND VL_${_name}_LIBRARY_DEBUG)
		set(VL_${_name}_LIBRARIES optimized ${VL_${_name}_LIBRARY} debug ${VL_${_name}_LIBRARY_DEBUG})
	elseif(VL_${_name}_LIBRARY)
		set(VL_${_name}_LIBRARIES optimized ${VL_${_name}_LIBRARY})
	elseif(VL_${_name}_LIBRARY_DEBUG)
		set(VL_${_name}_LIBRARIES debug ${VL_${_name}_LIBRARY_DEBUG})
	else()
		set(VL_${_name}_LIBRARIES "")
	endif()

	mark_as_advanced(VL_${_name}_LIBRARY VL_${_name}_LIBRARY_DEBUG)

	if(VL_${_name}_LIBRARIES)
		list(APPEND VL_LIBRARIES ${VL_${_name}_LIBRARIES})
	endif()
endmacro()

set(_vl_required_vars VL_INCLUDE_DIRS)
find_path(VL_INCLUDE_DIRS "vlCore/VisualizationLibrary.hpp"
	PATHS ${VL_ROOT} ENV VL_ROOT
	PATH_SUFFIXES "include" "include/vl"
)

# Find the requested VL components
foreach(component_name ${VL_FIND_COMPONENTS})
	_vl_find_component(${component_name})
	list(APPEND _vl_required_vars VL_${component_name}_LIBRARIES)
endforeach(component_name)

include( FindPackageHandleStandardArgs )
find_package_handle_standard_args(VL DEFAULT_MSG ${_vl_required_vars})
