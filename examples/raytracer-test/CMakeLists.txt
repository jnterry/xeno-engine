add_executable(raytracer-test
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(raytracer-test PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(raytracer-test PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(raytracer-test xen-all)

find_package(GLEW REQUIRED)
target_link_libraries(raytracer-test GLEW)
