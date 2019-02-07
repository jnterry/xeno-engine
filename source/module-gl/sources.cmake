set(XEN_HEADERS_GL
)

set(XEN_SOURCES_GL
  ${CMAKE_CURRENT_LIST_DIR}/src/gl_header.hxx

	${CMAKE_CURRENT_LIST_DIR}/src/Material.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Material.cpp

  ${CMAKE_CURRENT_LIST_DIR}/src/Mesh.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/Mesh.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/Texture.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/ModuleGl.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleGl.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/render.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render.cpp
)
