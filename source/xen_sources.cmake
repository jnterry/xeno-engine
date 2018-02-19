##################################################
## Core
##################################################
set(XEN_SOURCES_CORE
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/Allocator.cpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/ArenaLinear.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/random.cpp
)

set(XEN_HEADERS_CORE
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/intrinsics.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/Allocator.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/ArenaLinear.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/utilities.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/core/random.hpp
)

##################################################
## Util
##################################################
set(XEN_SOURCES_UTIL
  ${CMAKE_CURRENT_LIST_DIR}/xen/util/File.cpp
)

set(XEN_HEADERS_UTIL
  ${CMAKE_CURRENT_LIST_DIR}/xen/util/File.hpp
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
)


##################################################
## Graphics
##################################################
set(XEN_SOURCES_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Image.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.cpp
)

set(XEN_HEADERS_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Image.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/RenderCommand3d.hpp
)

##################################################
## SREN Render Backend
##################################################
set(XEN_SOURCES_SREN
  ${CMAKE_CURRENT_LIST_DIR}/xen/sren/renderer3d_raytracer.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/sren/renderer3d_rasterizer.cpp
)

set(XEN_HEADERS_SREN
    ${CMAKE_CURRENT_LIST_DIR}/xen/sren/renderer3d.hxx
)

##################################################
## GL Render Backend
##################################################
set(XEN_SOURCES_GL
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/Mesh.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Shader.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Texture.cpp
)

set(XEN_HEADERS_GL
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Shader.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/Mesh.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/gl/Texture.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/gl/gl_header.hxx
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
