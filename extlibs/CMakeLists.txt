use_cxx11()

# Don't rebuild extlibs if we change compiler options...
# This is to prevent rebuilds if we change settings such as
# whether to use xen double precision, xshm, etc.
#
# :TODO: Does this cause issues if we change settings such as
# optimizations - presumably we want to optimize the extlibs...
set(CMAKE_SKIP_RULE_DEPENDENCY ON)

# list(APPEND XEN_EXTLIB_TARGETS uninstall)

##########################################
# assimp
##########################################
if(";${XEN_BUILD_MODULES};" MATCHES ";graphics;")
	# Extract archive
	execute_process(COMMAND cmake -E tar xf assimp-5.2.5.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
	# Build libraries
	set(ASSIMP_BUILD_ASSIMP_TOOLS OFF)
	set(ASSIMP_BUILD_SAMPLES      OFF)
	set(ASSIMP_BUILD_TESTS        OFF)
	add_subdirectory(assimp-5.2.5)
	target_compile_definitions(assimp PRIVATE ASSIMP_BUILD_BOOST_WORKAROUND=1)

	list(APPEND XEN_EXTLIB_TARGETS assimp)

	if(WIN32)
	  list(APPEND XEN_EXTLIB_TARGETS UpdateAssimpLibsDebugSymbolsAndDLLs zlibstatic)
	endif()
endif()

##########################################
# GLEW
##########################################
if(";${XEN_BUILD_MODULES};" MATCHES ";gl;")
	# Extract archive
	execute_process(COMMAND cmake -E tar xf glew-2.1.0.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
	# Build librares
	add_subdirectory(glew-2.1.0)

	list(APPEND XEN_EXTLIB_TARGETS libglew_shared libglew_static libglewmx_shared libglewmx_static)
endif()

set(CMAKE_SKIP_RULE_DEPENDENCY OFF)

##########################################
# Set additional properties on extlibs
foreach(TARGET IN LISTS ${XEN_EXTLIB_TARGETS})
	set_target_properties(${TARGET} PROPERTIES FOLDER        "extlibs"     )

	# Disable warnings in extlibs by using default build flags rather than our custom ones
	set_target_properties(${TARGET} PROPERTIES COMPILE_FLAGS "${BUILD_FLAGS} -w")
endforeach()
