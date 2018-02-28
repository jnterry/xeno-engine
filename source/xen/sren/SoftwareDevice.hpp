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

	// :TODO: taking raw image as args to these is a hack -> should obey RenderTarget handle

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software raytracer
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena, RawImage& image);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software rasterizer
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena, RawImage& image);
}

#endif