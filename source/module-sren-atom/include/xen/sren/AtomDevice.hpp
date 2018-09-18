////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the SoftwareDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENATOM_ATOMDEVICE_HPP
#define XEN_MODULESRENATOM_ATOMDEVICE_HPP

#include <xen/core/array_types.hpp>

namespace xsr {
	class PostProcessor;
}

namespace xen {

	struct ArenaLinear;
	struct GraphicsDevice;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will perform rendering
	/// using a software atom based raytracer
	/// \param post_processors Array of post processors to to call before presenting
	/// the image to the screen. Defaults to an empty array
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createAtomTracerDevice(ArenaLinear& arena,
	                                       xen::Array<xsr::PostProcessor*> post_processors =
	                                       xen::Array<xsr::PostProcessor*>::EmptyArray
	                                      );


	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new GraphicsDevice which will present a debugged view
	/// of a standard atom renderer
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createAtomTracerDebugDevice(ArenaLinear& arena,
	                                            xen::Array<xsr::PostProcessor*> post_processors =
	                                            xen::Array<xsr::PostProcessor*>::EmptyArray
	                                           );
} //end of namespace xen

#endif
