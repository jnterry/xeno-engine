set(XEN_HEADERS_SREN
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/Framebuffer.hpp
)

set(XEN_SOURCES_SREN
	${CMAKE_CURRENT_LIST_DIR}/impl/xen/sren/RenderTarget.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Framebuffer.cpp
)
