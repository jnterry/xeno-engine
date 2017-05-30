set(XEN_CORE_SOURCES
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/Allocator.cpp
  ${CMAKE_CURRENT_LIST_DIR}/xen/core/memory/ArenaLinear.cpp
)

set(XEN_GRAPHICS_SOURCES
  ${CMAKE_CURRENT_LIST_DIR}/xen/graphics/Shader.gl.cpp
)

set(XEN_SOURCES
  ${XEN_CORE_SOURCES}
  ${XEN_GRAPHICS_SOURCES}
)
