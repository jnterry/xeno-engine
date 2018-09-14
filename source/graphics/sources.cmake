set(XEN_HEADERS_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Image.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Camera3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Color.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/RenderCommand3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Mesh_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Mesh.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/TestMeshes.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/GraphicsDevice_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/GraphicsDevice.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/graphics/Window.hpp
)

set(XEN_SOURCES_GRAPHICS
	${CMAKE_CURRENT_LIST_DIR}/impl/xen/graphics/Window.hxx

  ${CMAKE_CURRENT_LIST_DIR}/src/Image.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Camera3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Color.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RenderCommand3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/TestMeshes.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/GraphicsDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Window.cpp
)
