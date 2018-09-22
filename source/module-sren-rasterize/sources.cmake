set(XEN_HEADERS_MODULE_SREN_RASTERIZE
)

set(XEN_SOURCES_MODULE_SREN_RASTERIZE
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleRasterize.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleRasterize.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Mesh.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Shader.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Shader.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/render.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render.cpp
)
