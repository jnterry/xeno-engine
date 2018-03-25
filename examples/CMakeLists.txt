message(STATUS "Generating Xeno Engine Examples Build System")

# Adds an example application to the build process
# TARGET_NAME -> name of the executable to add
# All further parameters will be treated as source files for the application
function(xen_add_example TARGET_NAME)
	message("Adding example application: ${TARGET_NAME}")

	add_executable(${TARGET_NAME} ${ARGN})

	target_include_directories(${TARGET_NAME} PRIVATE ${XEN_INCLUDE_DIR})

	target_link_libraries(${TARGET_NAME} xen-all)

	target_include_directories(${TARGET_NAME} PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
	target_link_libraries(${TARGET_NAME} libglew_shared)

	# Group all examples together in IDE projects
	set_target_properties(${TARGET_NAME} PROPERTIES FOLDER "examples")

	# Ensure that we can run the target from visual studio debugger and find
	# resources in the bin directory
	set_target_properties(${TARGET_NAME} PROPERTIES
		VS_DEBUGGER_WORKING_DIRECTORY ${XEN_BIN_DIR}
	)
endfunction()

add_subdirectory(quicktest)
add_subdirectory(starfield)
add_subdirectory(raytracer-test)
add_subdirectory(cornell-box)
add_subdirectory(triangle-test)
add_subdirectory(line-test)
add_subdirectory(post-processing)
