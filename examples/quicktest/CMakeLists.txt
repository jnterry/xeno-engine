add_executable(quicktest
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
  ${CMAKE_CURRENT_LIST_DIR}/utilities.cpp
)

target_include_directories(quicktest PRIVATE ${PP_SFML_INCLUDE_DIR})
target_link_libraries(quicktest sfml-system)
target_link_libraries(quicktest sfml-window)

target_include_directories(quicktest PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(quicktest xen-all)

target_include_directories(quicktest PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_link_libraries(quicktest glew)
