set(XEN_HEADERS_MODULE_SREN_ATOM
)

set(XEN_SOURCES_MODULE_SREN_ATOM
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleAtom.cpp

	# For now we are being hacky and just reusing the rasterizer code
	# for most operations - if we need to start doing things differently
	# to the rasterizer (eg, pre-computing some things for meshes)
	# then we can replace the corresponding file here
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/RenderTarget.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Shader.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Mesh.cpp
)
