set(XEN_HEADERS_MODULE_SREN_ATOM
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/AtomDevice.hpp

)

set(XEN_SOURCES_MODULE_SREN_ATOM
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDebugDevice.cpp
)
