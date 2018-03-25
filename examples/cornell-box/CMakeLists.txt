add_executable(cornell-box
	${CMAKE_CURRENT_LIST_DIR}/cornell-box.cpp
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(cornell-box PRIVATE ${XEN_INCLUDE_DIR})

target_link_libraries(cornell-box xen-all)

target_include_directories(cornell-box PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(cornell-box libglew_shared)

set_target_properties(cornell-box PROPERTIES FOLDER "examples")
