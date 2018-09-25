message(STATUS "Generating Xeno Engine Examples Build System")

# Adds an example application to the build process
# TARGET_NAME -> name of the executable to add
# All further parameters will be treated as source files for the application
function(xen_add_example TARGET_NAME)
	message("Adding example application: ${TARGET_NAME}")

	add_executable(${TARGET_NAME} ${ARGN})

	target_include_directories(${TARGET_NAME} PRIVATE ${XEN_INCLUDE_DIR})



	if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
		target_link_libraries(${TARGET_NAME} PUBLIC -Wl,--whole-archive)
	endif()
	target_link_libraries(${TARGET_NAME} PUBLIC xen-core    )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-math    )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-kernel  )
	target_link_libraries(${TARGET_NAME} PUBLIC xen-graphics)
	target_link_libraries(${TARGET_NAME} PUBLIC xen-sren    )
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

	target_include_directories(${TARGET_NAME} PRIVATE ${XEN_INCLUDE_DIRS})

	# Group all examples together in IDE projects
	set_target_properties(${TARGET_NAME} PROPERTIES FOLDER "examples")
endfunction()

add_subdirectory(starfield)
add_subdirectory(quicktest)
add_subdirectory(cornell-box)
add_subdirectory(triangle-test)
add_subdirectory(line-test)
add_subdirectory(post-processing)
add_subdirectory(torus)
add_subdirectory(texture-test)
add_subdirectory(dynamic-reload)

xen_add_example(run-example ${CMAKE_CURRENT_LIST_DIR}/example-main.cpp)
