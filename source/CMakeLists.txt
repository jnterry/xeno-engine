message(STATUS "Generation Xeno Engine Build System")

include(xen_sources.cmake)

add_library(xen-all ${XEN_HEADERS_ALL} ${XEN_SOURCES_ALL})
target_include_directories(xen-all PUBLIC ${CMAKE_CURRENT_LIST_DIR})
