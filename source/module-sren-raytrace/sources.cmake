set(XEN_HEADERS_MODULE_SREN_RAYTRACE
)

set(XEN_SOURCES_MODULE_SREN_RAYTRACE
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleRaytrace.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.cpp

	# For now we are being hacky and just reusing the rasterizer code
	# for most operations - if we need to start doing things differently
	# to the rasterizer (eg, pre-computing some things for meshes)
	# then we can replace the corresponding file here
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/RenderTarget.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Shader.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/../module-sren-rasterize/src/Mesh.cpp
)
