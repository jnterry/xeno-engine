////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the SoftwareDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICE_HPP
#define XEN_SREN_SOFTWAREDEVICE_HPP

#include <xen/graphics/Image.hpp>

namespace xen {
	struct ArenaLinear;
	struct GraphicsDevice;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software raytracer
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software rasterizer
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a device which will debug the raytracer device by
	/// drawing the same scene with the rasterizer from multiple angles
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena);
}

#endif
