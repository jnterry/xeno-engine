set(XEN_HEADERS_MODULE_SREN
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/SoftwareDevice.hpp

)

set(XEN_SOURCES_MODULE_SREN

	${CMAKE_CURRENT_LIST_DIR}/src/render-utilities.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render-utilities.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/SoftwareDeviceBase.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/SoftwareDeviceBase.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/rasterizer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/rasterizer3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RasterizerDevice.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDebugDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDebugDevice.cpp
)
