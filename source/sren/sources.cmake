set(XEN_HEADERS_SREN
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/Framebuffer.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/Texture.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/PostProcessor.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/FragmentShader.hpp
)

set(XEN_SOURCES_SREN
	${CMAKE_CURRENT_LIST_DIR}/impl/xen/sren/SoftwareDeviceBase.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/SoftwareDeviceBase.cpp

	${CMAKE_CURRENT_LIST_DIR}/impl/xen/sren/RenderTarget.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTarget.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Framebuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/FragmentShader.cpp

	${CMAKE_CURRENT_LIST_DIR}/impl/xen/sren/rasterizer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/rasterizer3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/impl/xen/sren/render-debug.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render-debug.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/InvertColors.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/DisplayDepthBuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/Antialias.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/DepthFog.cpp
)
