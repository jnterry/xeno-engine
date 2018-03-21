////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the SoftwareDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICE_HPP
#define XEN_SREN_SOFTWAREDEVICE_HPP

#include <xen/sren/FrameBuffer.hpp>

namespace xen {
	struct ArenaLinear;
	struct GraphicsDevice;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software raytracer
	/// \param post_processors Array of post processors to call before presenting
	/// the image to the screen. Defaults to empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<sren::FrameBufferOperation> post_processors =
	                                      xen::Array<sren::FrameBufferOperation>::EmptyArray
	                                     );

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software rasterizer
	/// \param post_processors Array of post processors to to call before presenting
	/// the image to the screen. Defaults to an empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::FrameBufferOperation> post_processors =
	                                       xen::Array<sren::FrameBufferOperation>::EmptyArray
	                                      );

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a device which will debug the raytracer device by
	/// drawing the same scene with the rasterizer from multiple angles
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena);
}

#endif
