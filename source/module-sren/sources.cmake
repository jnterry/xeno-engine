set(XEN_HEADERS_SREN
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/SoftwareDevice.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/Framebuffer.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/PostProcessor.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/FragmentShader.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/sren/Texture.hpp

)

set(XEN_SOURCES_SREN

	${CMAKE_CURRENT_LIST_DIR}/src/render-utilities.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/render-utilities.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/FragmentShader.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Texture.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTargetImpl.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RenderTargetImpl.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/Framebuffer.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/InvertColors.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/DisplayDepthBuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/Antialias.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/post_processors/DepthFog.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/SoftwareDeviceBase.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/SoftwareDeviceBase.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/rasterizer3d.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/rasterizer3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RasterizerDevice.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/RaytracerDebugDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.hxx
  ${CMAKE_CURRENT_LIST_DIR}/src/raytracer3d.cpp

	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.hxx
	${CMAKE_CURRENT_LIST_DIR}/src/atomtracer.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDevice.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/AtomTracerDebugDevice.cpp
)
