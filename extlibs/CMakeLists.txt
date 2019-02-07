use_cxx17()

# Don't rebuild extlibs if we change compiler options...
# This is to prevent rebuilds if we change settings such as
# whether to use xen double precision, xshm, etc.
#
# :TODO: Does this cause issues if we change settings such as
# optimizations - presumably we want to optimize the extlibs...
set(CMAKE_SKIP_RULE_DEPENDENCY ON)

##########################################
# assimp
##########################################
# Extract archive
execute_process(COMMAND cmake -E tar xf assimp-3.3.1.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
# Build libraries
set(ASSIMP_BUILD_ASSIMP_TOOLS OFF)
set(ASSIMP_BUILD_SAMPLES      OFF)
set(ASSIMP_BUILD_TESTS        OFF)
add_subdirectory(assimp-3.3.1)
target_compile_definitions(assimp PRIVATE ASSIMP_BUILD_BOOST_WORKAROUND=1)

##########################################
# GLEW
##########################################
# Extract archive
execute_process(COMMAND cmake -E tar xf glew-2.1.0.zip WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
# Build librares
add_subdirectory(glew-2.1.0)

set(CMAKE_SKIP_RULE_DEPENDENCY OFF)


##########################################
# Set additional properties on extlibs

set(XEN_EXTLIB_TARGETS
	assimp
	uninstall
	libglew_shared
	libglew_static
	libglewmx_shared
	libglewmx_static
)
if(WIN32)
	set(XEN_EXTLIB_TARGETS ${XEN_EXTLIB_TARGETS}
		UpdateAssimpLibsDebugSymbolsAndDLLs
		zlibstatic
	)
endif()

foreach(TARGET ${XEN_EXTLIB_TARGETS})
	set_target_properties(${TARGET} PROPERTIES FOLDER        "extlibs"     )

	# Disable warnings in extlibs by using default build flags rather than our custom ones
	set_target_properties(${TARGET} PROPERTIES COMPILE_FLAGS "${BUILD_FLAGS} -w")
endforeach()
