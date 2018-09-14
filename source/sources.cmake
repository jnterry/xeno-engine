include(core/sources.cmake)
include(graphics/sources.cmake)
include(kernel/sources.cmake)
include(math/sources.cmake)
include(module-gl/sources.cmake)
include(module-sren/sources.cmake)

set(XEN_SOURCES_ALL
  ${XEN_SOURCES_CORE}
  ${XEN_SOURCES_MATH}
	${XEN_SOURCES_KERNEL}
  ${XEN_SOURCES_GRAPHICS}
	${XEN_SOURCES_MODULE_SREN}
	${XEN_SOURCES_MODULE_GL}
)

set(XEN_HEADERS_ALL
  ${XEN_HEADERS_CORE}
  ${XEN_HEADERS_MATH}
	${XEN_SOURCES_KERNEL}
  ${XEN_HEADERS_GRAPHICS}
	${XEN_HEADERS_MODULE_SREN}
	${XEN_SOURCES_MODULE_GL}
)
