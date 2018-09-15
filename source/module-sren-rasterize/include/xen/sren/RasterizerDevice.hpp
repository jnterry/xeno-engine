////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_RASTERIZEDEVICE_HPP
#define XEN_MODULESRENRASTERIZE_RASTERIZEDEVICE_HPP

#include <xen/core/array_types.hpp>

namespace xsren {
	class PostProcessor;
}

namespace xen {
	struct ArenaLinear;
	struct GraphicsDevice;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software rasterizer
	/// \param post_processors Array of post processors to to call before presenting
	/// the image to the screen. Defaults to an empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<xsren::PostProcessor*> post_processors =
	                                       xen::Array<xsren::PostProcessor*>::EmptyArray
	                                      );
}

#endif
