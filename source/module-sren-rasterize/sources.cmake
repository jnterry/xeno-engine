set(XEN_HEADERS_MODULE_SREN_RASTERIZE
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/RasterizerDevice.hpp
)

set(XEN_SOURCES_MODULE_SREN_RASTERIZE
	${CMAKE_CURRENT_LIST_DIR}/src/RasterizerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleRasterize.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleRasterize.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Mesh.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp
)
