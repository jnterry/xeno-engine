set(XEN_HEADERS_MODULE_SREN_RAYTRACE
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/RaytracerDevice.hpp
)

set(XEN_SOURCES_MODULE_SREN_RAYTRACE
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDebugDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.cpp
)
