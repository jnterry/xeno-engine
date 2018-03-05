add_executable(triangle-test
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(triangle-test PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(triangle-test PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(triangle-test xen-all)
target_link_libraries(triangle-test SDL2)
target_link_libraries(triangle-test SDL2main)

find_package(GLEW REQUIRED)
target_link_libraries(triangle-test GLEW)
