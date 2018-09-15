message(STATUS "Generating Xeno Engine Build System")

link_directories(${XEN_LIB_DIR})

include(sources.cmake)

add_library               (xen-core STATIC  ${XEN_HEADERS_CORE} ${XEN_SOURCES_CORE})
target_include_directories(xen-core PUBLIC  ${CMAKE_CURRENT_LIST_DIR}/core/include/)
target_include_directories(xen-core PRIVATE ${CMAKE_CURRENT_LIST_DIR}/core/impl/)


add_library               (xen-math STATIC ${XEN_HEADERS_MATH} ${XEN_SOURCES_MATH} )
target_include_directories(xen-math PUBLIC ${CMAKE_CURRENT_LIST_DIR}/core/include/)
target_include_directories(xen-math PUBLIC ${CMAKE_CURRENT_LIST_DIR}/math/include/)


add_library               (xen-kernel STATIC ${XEN_HEADERS_KERNEL} ${XEN_SOURCES_KERNEL})
target_include_directories(xen-kernel PUBLIC ${CMAKE_CURRENT_LIST_DIR}/core/include/  )
target_include_directories(xen-kernel PUBLIC ${CMAKE_CURRENT_LIST_DIR}/kernel/include/)
if(UNIX)
	target_link_libraries(xen-kernel dl)
endif()


add_library               (xen-graphics STATIC ${XEN_HEADERS_GRAPHICS} ${XEN_SOURCES_GRAPHICS})
target_include_directories(xen-graphics PRIVATE ${XEN_EXTLIBS_DIR}/stb/)
target_include_directories(xen-graphics PRIVATE ${XEN_EXTLIBS_DIR}/assimp-3.3.1/include/)
target_include_directories(xen-graphics PRIVATE ${CMAKE_CURRENT_LIST_DIR}/graphics/impl/)
target_include_directories(xen-graphics PUBLIC  ${CMAKE_CURRENT_LIST_DIR}/graphics/include/)
target_link_libraries     (xen-graphics PUBLIC  xen-core)
target_link_libraries     (xen-graphics PUBLIC  xen-math)
target_link_libraries     (xen-graphics PRIVATE assimp  )
if(UNIX)
	target_link_libraries(xen-graphics X11)
endif()


add_library               (xen-module-sren STATIC  ${XEN_HEADERS_SREN} ${XEN_SOURCES_SREN})
target_include_directories(xen-module-sren PUBLIC  ${CMAKE_CURRENT_LIST_DIR}/module-sren/include/)
target_include_directories(xen-module-sren PRIVATE ${XEN_EXTLIBS_DIR}/thpool/)
target_include_directories(xen-module-sren PRIVATE ${CMAKE_CURRENT_LIST_DIR}/graphics/impl/)
target_link_libraries     (xen-module-sren xen-graphics)
target_link_libraries     (xen-module-sren PRIVATE thpool)
target_link_libraries     (xen-module-sren PRIVATE pthread)


add_library               (xen-module-gl STATIC  ${XEN_HEADERS_GL} ${XEN_SOURCES_GL})
target_include_directories(xen-module-gl PUBLIC  ${CMAKE_CURRENT_LIST_DIR}/module-gl/include/)
target_include_directories(xen-module-gl PRIVATE ${XEN_EXTLIBS_DIR}/glew-2.1.0/include/)
target_include_directories(xen-module-gl PRIVATE ${CMAKE_CURRENT_LIST_DIR}/graphics/impl/)
target_link_libraries     (xen-module-gl libglew_shared)
target_link_libraries     (xen-module-gl xen-graphics)
target_link_libraries     (xen-module-gl xen-kernel  )


set_target_properties(xen-core         PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-math         PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-graphics     PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-module-gl    PROPERTIES FOLDER "xeno_engine")
set_target_properties(xen-module-sren  PROPERTIES FOLDER "xeno_engine")
