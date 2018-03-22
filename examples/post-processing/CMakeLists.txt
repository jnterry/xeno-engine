add_executable(post-processing
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(post-processing PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(post-processing xen-all)

target_include_directories(post-processing PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(post-processing libglew_shared)
