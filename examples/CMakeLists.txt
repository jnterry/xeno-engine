message(STATUS "Generating Xeno Engine Examples Build System")

# Adds an example application to the build process
# TARGET_NAME -> name of the executable to add
# All further parameters will be treated as source files for the application
function(xen_add_example TARGET_NAME)
	message("Adding example application: ${TARGET_NAME}")

	add_executable(${TARGET_NAME} ${ARGN})

	target_include_directories(${TARGET_NAME} PRIVATE ${XEN_INCLUDE_DIR})

	if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
		 # The modules we load expect to find the symbols decarled in these
		 # libraries in the global symbol table of the executable, but symbols
		 # wont be included in the executable if they are not used by the executable...
		 # unless we specify this flag
		target_link_libraries(${TARGET_NAME} PUBLIC -Wl,--whole-archive)
	endif()
	target_link_libraries(${TARGET_NAME} PUBLIC xen-core    )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-math    )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-kernel  )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-graphics)
	# target_link_libraries(${TARGET_NAME} PUBLIC xen-sren    )
	if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
		target_link_libraries(${TARGET_NAME} PUBLIC -Wl,--no-whole-archive)
	endif()

	# Group all examples together in IDE projects
	set_target_properties(${TARGET_NAME} PROPERTIES FOLDER "examples")

	# Ensure that we can run the target from visual studio debugger and find
	# resources in the bin directory
	set_target_properties(${TARGET_NAME} PROPERTIES
		VS_DEBUGGER_WORKING_DIRECTORY ${XEN_BIN_DIR}
	)
endfunction()

set(_THIS_DIR ${CMAKE_CURRENT_LIST_DIR})

# Adds an example application to the build process
# TARGET_NAME -> name of the executable to add
# All further parameters will be treated as source files for the application
function(xen_add_example_module TARGET_NAME)
	message("Adding example application module: ${TARGET_NAME}")

	add_library(${TARGET_NAME} SHARED
		${ARGN}
		${_THIS_DIR}/utilities.hpp
		${_THIS_DIR}/utilities.cpp
	)

	if(MSVC)
		# Under windows module shared libraries cannot call functions linked
		# into the executable, so modules must link against core xen code
		# (except for the kernel, which is handled in a special way to avoid
		# duplicate copies of kernel global state)
		target_link_libraries(${TARGET_NAME} PUBLIC xen-core)
		target_link_libraries(${TARGET_NAME} PUBLIC xen-math)
		target_link_libraries(${TARGET_NAME} PUBLIC xen-graphics)
		# target_link_libraries(${TARGET_NAME} PUBLIC xen-sren)
	endif()

	target_include_directories(${TARGET_NAME} PRIVATE ${XEN_INCLUDE_DIRS})

	# Group all examples together in IDE projects
	set_target_properties(${TARGET_NAME} PROPERTIES FOLDER "examples")
endfunction()

set(XEN_BUILD_EXAMPLE_MODULES TRUE CACHE BOOL "If set example modules will be built")
if("${XEN_BUILD_EXAMPLE_MODULES}")
	add_subdirectory(starfield)
	add_subdirectory(quicktest)
	add_subdirectory(cornell-box)
	#add_subdirectory(triangle-test)
	# add_subdirectory(line-test)
	# add_subdirectory(torus)
	add_subdirectory(texture-test)
	add_subdirectory(dynamic-reload)
endif()

set(XEN_BUILD_EXAMPLE_RUNNER TRUE CACHE BOOL "If set then example runner application main will be built")
if("${XEN_BUILD_EXAMPLE_RUNNER}")
	xen_add_example(run-example ${CMAKE_CURRENT_LIST_DIR}/example-main.cpp)
endif()
