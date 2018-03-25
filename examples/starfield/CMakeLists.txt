add_executable(starfield
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

target_include_directories(starfield PRIVATE ${XEN_INCLUDE_DIR})
target_include_directories(starfield PRIVATE "${XEN_EXTLIBS_DIR}/SDL2-2.0.7/include")

target_link_libraries(starfield xen-all)

target_include_directories(starfield PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(starfield libglew_shared)

set_target_properties(starfield PROPERTIES FOLDER "examples")
