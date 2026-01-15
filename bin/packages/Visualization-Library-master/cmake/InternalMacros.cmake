################################################################################
# Required CMake Modules
################################################################################
include(ParseArguments)
include(CMakeDependentOption)

################################################################################
# Macro to register extra dependencies for a project
################################################################################
macro(VL_PROJECT_ADD projectName)
	cmake_parse_arguments(EXTRA "" "" "SOURCES;DEFINITIONS;INCLUDE_DIRECTORIES;LINK_LIBRARIES;LINK_LIBRARIES_DEBUG;LINK_LIBRARIES_RELEASE" ${ARGN})
	set(projectDir "${${projectName}_SOURCE_DIR}")
	list(APPEND EXTRA_LINK_LIBRARIES_DEBUG ${EXTRA_LINK_LIBRARIES})
	list(APPEND EXTRA_LINK_LIBRARIES_RELEASE ${EXTRA_LINK_LIBRARIES})
	set_property(DIRECTORY ${projectDir} APPEND PROPERTY EXTRA_SOURCES "${EXTRA_SOURCES}")
	set_property(DIRECTORY ${projectDir} APPEND PROPERTY EXTRA_DEFINITIONS "${EXTRA_DEFINITIONS}")
	set_property(DIRECTORY ${projectDir} APPEND PROPERTY EXTRA_INCLUDE_DIRECTORIES "${EXTRA_INCLUDE_DIRECTORIES}")
	set_property(DIRECTORY ${projectDir} APPEND PROPERTY EXTRA_LINK_LIBRARIES_DEBUG "${EXTRA_LINK_LIBRARIES_DEBUG}")
	set_property(DIRECTORY ${projectDir} APPEND PROPERTY EXTRA_LINK_LIBRARIES_RELEASE "${EXTRA_LINK_LIBRARIES_RELEASE}")
endmacro(VL_PROJECT_ADD)

################################################################################
# Macro to retrieve the extra dependencies of a project
################################################################################
macro(VL_PROJECT_GET projectName sourcesVar definitionsVar includeDirsVar linkLibrariesDebugVar linkLibrariesReleaseVar )
	set(projectDir "${${projectName}_SOURCE_DIR}")
	get_property(${sourcesVar} DIRECTORY ${projectDir} PROPERTY EXTRA_SOURCES)
	get_property(${definitionsVar} DIRECTORY ${projectDir} PROPERTY EXTRA_DEFINITIONS)
	get_property(${includeDirsVar} DIRECTORY ${projectDir} PROPERTY EXTRA_INCLUDE_DIRECTORIES)
	get_property(${linkLibrariesDebugVar} DIRECTORY ${projectDir} PROPERTY EXTRA_LINK_LIBRARIES_DEBUG)
	get_property(${linkLibrariesReleaseVar} DIRECTORY ${projectDir} PROPERTY EXTRA_LINK_LIBRARIES_RELEASE)
endmacro(VL_PROJECT_GET)

################################################################################
# Macro to set common properties for all targets
################################################################################
macro(VL_DEFAULT_TARGET_PROPERTIES targetName)
	set_target_properties(${targetName} PROPERTIES
		SOVERSION "${VLVERSION}"
		VERSION   "${VLVERSION}"
	)
endmacro(VL_DEFAULT_TARGET_PROPERTIES)

################################################################################
# Macro to process a list of optional compile-time plugins for a project
################################################################################
#
# Defines a CMake option for every plugin in a list, then adds the enabled
# plugins to a specified project. For every enabled plugin, the project gets
# a preprocessor definition based on a prefix and the plugin's name, as well as
# an extra pair o sources (named "io${pluginName}.h/cpp" in the current dir).
#
# Parameters:
#    projectName    Name of the project the plugins should be added to.
#    prefix         A common prefix for the plugin options and definitions.
#    plugins		A list of plugin names.
#
# A plugin's option description and default value may be customized by setting
# a special variable: set({PREFIX_}{PLUGINNAME}_OPTION "Description" value).
# By default, all plugin options default to ON.
#
macro(VL_PROCESS_PROJECT_PLUGINS projectName prefix installDir)
	foreach(pluginName ${ARGN})
		set(prefixedName ${prefix}${pluginName})
		if(NOT DEFINED ${prefixedName}_OPTION)
			set(${prefixedName}_OPTION "Enable ${pluginName} support" ON)
		endif()
		option(${prefixedName} ${${prefixedName}_OPTION})
		if(${prefixedName})
			VL_PROJECT_ADD(${projectName}
				DEFINITIONS "-D${prefixedName}"
				SOURCES
					"${CMAKE_CURRENT_SOURCE_DIR}/io${pluginName}.hpp"
					"${CMAKE_CURRENT_SOURCE_DIR}/io${pluginName}.cpp"
			)
		install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/io${pluginName}.hpp" DESTINATION ${installDir})
		endif()
	endforeach()
endmacro(VL_PROCESS_PROJECT_PLUGINS)

################################################################################
# Macro to install a target
################################################################################
macro(VL_INSTALL_TARGET targetName)
	install( TARGETS ${targetName}
		RUNTIME DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
		LIBRARY DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
		ARCHIVE DESTINATION ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
	)
endmacro(VL_INSTALL_TARGET)

################################################################################
# Adds a file to the list of files to be removed by "make clean" in a directory
################################################################################
macro(VL_ADD_TO_MAKE_CLEAN filename)
	set_property( DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "${filename}" )
endmacro(VL_ADD_TO_MAKE_CLEAN)

################################################################################
# Doxygen Macro
################################################################################
# Creates an optional target to generate documentation using Doxygen.
#
# This macro will preprocess the file "${CMAKE_CURRENT_SOURCE_DIR}/${doxyfileName}"
# using CONFIGURE_FILE(), save the result to ${CMAKE_CURRENT_BINARY_DIR}, set the
# OUTPUT_DIRECTORY option to ${CMAKE_CURRENT_BINARY_DIR} and then run Doxygen on
# the file using ${CMAKE_CURRENT_SOURCE_DIR} as the working directory.
#
# Parameters:
#    targetName     Name given to the custom target.
#    doxyfileName   Name of the Doxygen config file in the current source dir.
#
# Extra arguments are passed along to the custom target command.
#
MACRO( VL_GENERATE_DOXYGEN targetName doxyfileName )

	FIND_PACKAGE( Doxygen )

	IF( DOXYGEN_FOUND )

		SET( DOXYFILE_FOUND false )
		IF( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${doxyfileName}" )
			SET( DOXYFILE_FOUND true )
		ENDIF()

		IF( DOXYFILE_FOUND )

			MESSAGE( STATUS "Setting up Doxygen target '${targetName}'..." )

			SET( DOXYFILE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${doxyfileName}.configured" )
			CONFIGURE_FILE( "${CMAKE_CURRENT_SOURCE_DIR}/${doxyfileName}" "${DOXYFILE_PATH}" )
			FILE( APPEND "${DOXYFILE_PATH}" "OUTPUT_DIRECTORY = ${CMAKE_CURRENT_BINARY_DIR}" )

			ADD_CUSTOM_TARGET( ${targetName}
				COMMENT "Generating API documentation with Doxygen..."
				WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
				COMMAND ${DOXYGEN_EXECUTABLE} "${DOXYFILE_PATH}" ${ARGN}
			)

			SET_TARGET_PROPERTIES( ${targetName} PROPERTIES PROJECT_LABEL "Doxygen" )

			# Add .tag file and generated documentation to the list of files we must erase when distcleaning

			# Read doxygen configuration file
			FILE( READ "${DOXYFILE_PATH}" DOXYFILE_CONTENTS )
			STRING( REGEX REPLACE "\n" ";" DOXYFILE_LINES ${DOXYFILE_CONTENTS} )

			# Parse .tag filename and add to list of files to delete if it exists
			SET( DOXYGEN_TAG_FILE )
			SET( DOXYGEN_OUTPUT_DIRECTORY )
			SET( DOXYGEN_HTML_OUTPUT )

			FOREACH( DOXYLINE ${DOXYFILE_LINES} )
				STRING( REGEX REPLACE ".*GENERATE_TAGFILE *= *([^^\n]+).*" "\\1" DOXYGEN_TAG_FILE			"${DOXYLINE}" )
				STRING( REGEX REPLACE ".*OUTPUT_DIRECTORY *= *([^^\n]+).*" "\\1" DOXYGEN_OUTPUT_DIRECTORY	"${DOXYLINE}" )
				STRING( REGEX REPLACE ".*HTML_OUTPUT *= *([^^\n]+).*" "\\1"		 DOXYGEN_HTML_OUTPUT		"${DOXYLINE}" )
			ENDFOREACH( DOXYLINE )

			FILE( TO_CMAKE_PATH "${DOXYGEN_OUTPUT_DIRECTORY}" DOXYGEN_OUTPUT_DIRECTORY )
			FILE( TO_CMAKE_PATH "${DOXYGEN_HTML_OUTPUT}" DOXYGEN_HTML_OUTPUT )

			IF( DOXYGEN_TAG_FILE )
				VL_ADD_TO_MAKE_CLEAN( "${CMAKE_CURRENT_BINARY_DIR}/${DOXYGEN_TAG_FILE}" )
			ENDIF()

			IF( DOXYGEN_OUTPUT_DIRECTORY AND DOXYGEN_HTML_OUTPUT )
				VL_ADD_TO_MAKE_CLEAN( "${DOXYGEN_OUTPUT_DIRECTORY}/${DOXYGEN_HTML_OUTPUT}" )
			ENDIF()

		ELSE( DOXYFILE_FOUND )
			MESSAGE( SEND_ERROR "Doxyfile not found - configuration error." )
		ENDIF( DOXYFILE_FOUND )

	ELSE( DOXYGEN_FOUND )
		MESSAGE( "Doxygen not found - documentation will not be generated." )
	ENDIF( DOXYGEN_FOUND )

ENDMACRO( VL_GENERATE_DOXYGEN )
