include(xen_sources.cmake)

message(STATUS "core sources are: ${XEN_CORE_SOURCES}")
add_executable(ProcedualPlanet ${CMAKE_CURRENT_LIST_DIR}/main.cpp ${XEN_CORE_SOURCES})

target_include_directories(ProcedualPlanet PRIVATE ${PP_SFML_INCLUDE_DIR})
target_include_directories(ProcedualPlanet PRIVATE ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries(ProcedualPlanet sfml-system)
target_link_libraries(ProcedualPlanet sfml-window)
