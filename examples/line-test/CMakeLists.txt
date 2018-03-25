add_executable(line-test
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(line-test PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(line-test PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(line-test xen-all)

target_include_directories(line-test PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(line-test libglew_shared)
