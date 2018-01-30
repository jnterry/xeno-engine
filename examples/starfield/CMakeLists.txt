add_executable(starfield
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(starfield PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(starfield PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(starfield xen-all)
target_link_libraries(starfield SDL2)
target_link_libraries(starfield SDL2main)

find_package(GLEW REQUIRED)
target_link_libraries(starfield GLEW)
