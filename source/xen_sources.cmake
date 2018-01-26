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
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/Quaternion.cpp
  )

set(XEN_HEADERS_MATH
	${CMAKE_CURRENT_LIST_DIR}/xen/math/utilities.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/Angle.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/Vector.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/Matrix.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/math/Quaternion.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/geometry_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/math/geometry.hpp
)


##################################################
## Graphics
##################################################
set(XEN_SOURCES_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Shader.gl.cpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Mesh.cpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.cpp
)

set(XEN_HEADERS_GRAPHICS
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/gl_header.hxx
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Shader.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Mesh.hpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Texture.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Camera3d.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Color.hpp
	${CMAKE_CURRENT_LIST_DIR}/xen/graphics/RenderCommand3d.hpp
)

##################################################
## SREN
##################################################
set(XEN_SOURCES_SREN
  ${CMAKE_CURRENT_LIST_DIR}/xen/sren/renderer3d.cpp
)

set(XEN_HEADERS_SREN
    ${CMAKE_CURRENT_LIST_DIR}/xen/sren/renderer3d.hxx
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
)

set(XEN_HEADERS_ALL
  ${XEN_HEADERS_CORE}
  ${XEN_HEADERS_UTIL}
  ${XEN_HEADERS_MATH}
  ${XEN_HEADERS_GRAPHICS}
	${XEN_HEADERS_SREN}
)
