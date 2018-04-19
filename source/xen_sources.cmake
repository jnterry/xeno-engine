##################################################
## Core
##################################################
set(XEN_SOURCES_CORE
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/Allocator.cpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/ArenaLinear.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/random.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/time.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/File.cpp
)

set(XEN_HEADERS_CORE
	${CMAKE_CURRENT_LIST_DIR}/xen/config.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/windows_header.hxx
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/intrinsics.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/Allocator.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/ArenaLinear.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/utilities.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/random.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/time.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/File.hpp
)

##################################################
## Math
##################################################
set(XEN_SOURCES_MATH
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/quaternion.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/impl/matrix_arithmetic.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/math/impl/matrix_operations.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/math/impl/matrix_transforms.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/math/impl/vector_arithmetic.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/math/impl/vector_operations.hxx
)

set(XEN_HEADERS_MATH
	${CMAKE_CURRENT_LIST_DIR}/xen/math/utilities.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/angle.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/vector_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/vector.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/matrix_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/matrix.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/quaternion_types.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/quaternion.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/geometry_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/geometry.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/vertex_group_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/vertex_group.hpp
)


##################################################
## Graphics
##################################################
set(XEN_SOURCES_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Image.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/RenderCommand3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/TestMeshes.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/GraphicsDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Window.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Window.cpp
)

set(XEN_HEADERS_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Image.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/RenderCommand3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Mesh.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/TestMeshes.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/GraphicsDevice_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/GraphicsDevice.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Window.hpp
)

##################################################
## SREN Render Backend
##################################################
set(XEN_SOURCES_SREN
  ${CMAKE_CURRENT_LIST_DIR}/xen/sren/raytracer3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/rasterizer3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/render-utilities.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/SoftwareDeviceBase.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RaytracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RaytracerDebugDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RasterizerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RenderTargetImpl.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/Framebuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/post_processors/InvertColors.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/post_processors/DisplayDepthBuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/FragmentShader.cpp
)

set(XEN_HEADERS_SREN
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/SoftwareDevice.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/Framebuffer.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/PostProcessor.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/SoftwareDeviceBase.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RaytracerDevice.hxx
  ${CMAKE_CURRENT_LIST_DIR}/xen/sren/rasterizer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/raytracer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/rasterizer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/render-utilities.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/RenderTargetImpl.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/FragmentShader.hpp
)

##################################################
## GL Render Backend
##################################################
set(XEN_SOURCES_GL
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Shader.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/GlDevice.cpp
)

set(XEN_HEADERS_GL
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Shader.hxx
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/Mesh.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Texture.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/gl_header.hxx
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/GlDevice.hpp
)

##################################################
## All
##################################################
set(XEN_SOURCES_ALL
  ${XEN_SOURCES_CORE}
  ${XEN_SOURCES_UTIL}
  ${XEN_SOURCES_MATH}
  ${XEN_SOURCES_GRAPHICS}
	${XEN_SOURCES_SREN}
	${XEN_SOURCES_GL}
)

set(XEN_HEADERS_ALL
  ${XEN_HEADERS_CORE}
  ${XEN_HEADERS_UTIL}
  ${XEN_HEADERS_MATH}
  ${XEN_HEADERS_GRAPHICS}
	${XEN_HEADERS_SREN}
	${XEN_SOURCES_GL}
)
