add_executable(ProcedualPlanet
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp
  ${CMAKE_CURRENT_LIST_DIR}/Camera3d.cpp
  ${CMAKE_CURRENT_LIST_DIR}/utilities.cpp
)

target_include_directories(ProcedualPlanet PRIVATE ${PP_SFML_INCLUDE_DIR})

message(STATUS "Xen include dir: ${XEN_INCLUDE_DIR}")
target_include_directories(ProcedualPlanet PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(ProcedualPlanet sfml-system)
target_link_libraries(ProcedualPlanet sfml-window)
target_link_libraries(ProcedualPlanet xen-all)

find_package(GLEW REQUIRED)
target_link_libraries(ProcedualPlanet GLEW)
