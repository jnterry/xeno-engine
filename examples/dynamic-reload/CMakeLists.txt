xen_add_example(dynamic-reload
	${CMAKE_CURRENT_LIST_DIR}/main.cpp
)

add_library(dynamic-reload-game SHARED ${CMAKE_CURRENT_LIST_DIR}/game.cpp)
target_include_directories(dynamic-reload-game PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(dynamic-reload-game xen-all)
