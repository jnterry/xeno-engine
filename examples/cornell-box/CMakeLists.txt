add_executable(cornell-box
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
	${CMAKE_CURRENT_LIST_DIR}/testModel.cpp
)

target_include_directories(cornell-box PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(cornell-box PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(cornell-box xen-all)
target_link_libraries(cornell-box SDL2)
target_link_libraries(cornell-box SDL2main)

target_include_directories(cornell-box PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(cornell-box libglew_shared)
