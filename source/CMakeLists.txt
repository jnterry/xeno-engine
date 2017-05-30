add_executable            (ProcedualPlanet ${CMAKE_CURRENT_LIST_DIR}/main.cpp ${CMAKE_CURRENT_LIST_DIR}/core/memory/Allocator.cpp)

MESSAGE( STATUS "SFML include dir is: ${PP_SFML_INCLUDE_DIR}")
target_include_directories(ProcedualPlanet PRIVATE ${PP_SFML_INCLUDE_DIR})
target_link_libraries(ProcedualPlanet sfml-system)
target_link_libraries(ProcedualPlanet sfml-window)
