message(STATUS "Generation Xeno Engine Build System")

link_directories(${XEN_LIB_DIR})

include(xen_sources.cmake)

add_library(xen-all ${XEN_HEADERS_ALL} ${XEN_SOURCES_ALL})
target_include_directories(xen-all PRIVATE "${XEN_EXTLIBS_DIR}/stb/")
target_include_directories(xen-all PRIVATE "${XEN_EXTLIBS_DIR}/assimp-3.3.1/include/")
target_include_directories(xen-all PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_include_directories(xen-all PUBLIC ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-all libglew_shared)
target_link_libraries     (xen-all assimp)

add_library(xen-core ${XEN_HEADERS_CORE} ${XEN_SOURCES_CORE})
target_include_directories(xen-core PUBLIC ${CMAKE_CURRENT_LIST_DIR})

add_library(xen-util ${XEN_HEADERS_UTIL} ${XEN_SOURCES_UTIL})
target_include_directories(xen-util PUBLIC ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-util xen-core)

add_library(xen-math ${XEN_HEADERS_MATH} ${XEN_SOURCES_MATH})
target_include_directories(xen-math PUBLIC ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-math xen-core)

add_library(xen-graphics ${XEN_HEADERS_GRAPHICS} ${XEN_SOURCES_GRAPHICS})
target_include_directories(xen-graphics PRIVATE "${XEN_EXTLIBS_DIR}/stb/")
target_include_directories(xen-graphics PRIVATE "${XEN_EXTLIBS_DIR}/assimp-3.3.1/include/")
target_include_directories(xen-graphics PUBLIC  ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-graphics xen-core)
target_link_libraries     (xen-graphics xen-math)
target_link_libraries     (xen-graphics assimp  )

if(UNIX)
	target_link_libraries(xen-graphics X11)
endif()

add_library(xen-sren ${XEN_HEADERS_SREN} ${XEN_SOURCES_SREN})
target_include_directories(xen-sren PUBLIC  ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-sren xen-graphics)

add_library(xen-gl ${XEN_HEADERS_GL} ${XEN_SOURCES_GL})
target_include_directories(xen-gl PRIVATE "${XEN_EXTLIBS_DIR}/glew-2.1.0/include/")
target_include_directories(xen-gl PUBLIC  ${CMAKE_CURRENT_LIST_DIR})
target_link_libraries     (xen-gl libglew_shared)
target_link_libraries     (xen-gl xen-graphics)

set_target_properties(xen-all      PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-core     PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-util     PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-math     PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-graphics PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-gl       PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-sren     PROPERTIES FOLDER "xeno_engine")
