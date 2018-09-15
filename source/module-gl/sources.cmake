set(XEN_HEADERS_GL
	${CMAKE_CURRENT_LIST_DIR}/include/xen/gl/GlDevice.hpp
)

set(XEN_SOURCES_GL
  ${CMAKE_CURRENT_LIST_DIR}/src/gl_header.hxx

	${CMAKE_CURRENT_LIST_DIR}/src/GlDevice.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/Shader.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Shader.cpp

  ${CMAKE_CURRENT_LIST_DIR}/src/Mesh.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/Mesh.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/Texture.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/ModuleGl.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/ModuleGl.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/Window.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/Window.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/render.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render.cpp
)
