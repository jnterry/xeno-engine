message(STATUS "Generation Xeno Engine Build System")

include(xen_sources.cmake)

add_library(xen-all ${XEN_HEADERS_ALL} ${XEN_SOURCES_ALL})
target_include_directories(xen-all PUBLIC  ${CMAKE_CURRENT_LIST_DIR})
target_include_directories(xen-all PRIVATE "${XEN_EXTLIBS_DIR}/TinyObjLoader/")

add_library(xen-core ${XEN_HEADERS_CORE} ${XEN_SOURCES_CORE})
target_include_directories(xen-core PUBLIC ${CMAKE_CURRENT_LIST_DIR})

add_library(xen-util ${XEN_HEADERS_UTIL} ${XEN_SOURCES_UTIL})
target_include_directories(xen-util PUBLIC ${CMAKE_CURRENT_LIST_DIR})

add_library(xen-math ${XEN_HEADERS_MATH} ${XEN_SOURCES_MATH})
target_include_directories(xen-math PUBLIC ${CMAKE_CURRENT_LIST_DIR})

add_library(xen-graphics ${XEN_HEADERS_GRAPHICS} ${XEN_SOURCES_GRAPHICS})
message(STATUS "extlibs: ${XEN_EXTLIBS_DIR}")
target_include_directories(xen-graphics PRIVATE "${XEN_EXTLIBS_DIR}/TinyObjLoader/")
target_include_directories(xen-graphics PUBLIC  ${CMAKE_CURRENT_LIST_DIR})
