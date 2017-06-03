add_executable(quicktest
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Camera3d.cpp
  ${CMAKE_CURRENT_LIST_DIR}/utilities.cpp
)

target_include_directories(quicktest PRIVATE ${PP_SFML_INCLUDE_DIR})
target_link_libraries(quicktest sfml-system)
target_link_libraries(quicktest sfml-window)

target_include_directories(quicktest PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(quicktest xen-all)

find_package(GLEW REQUIRED)
target_link_libraries(quicktest GLEW)
