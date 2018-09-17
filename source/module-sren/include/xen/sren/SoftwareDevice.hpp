////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the SoftwareDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICE_HPP
#define XEN_SREN_SOFTWAREDEVICE_HPP

#include <xen/core/array_types.hpp>
#include <xen/math/vector_types.hpp>

namespace xsr {
	class PostProcessor;
}

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
	                                      xen::Array<xsr::PostProcessor*> post_processors =
	                                      xen::Array<xsr::PostProcessor*>::EmptyArray
	                                      );

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a device which will debug the raytracer device by
	/// drawing the same scene with the rasterizer from multiple angles
	/// \param camera_distance Distance along each axis that debug cameras should
	/// be placed at from the camera_center
	/// \param camera_center Center point that debug cameras should look at. If
	/// set to nullptr then position of the main render camera will be used as
	/// center
	/////////////////////////////////////////////////////////////////////
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena,
	                                           real         camera_distance = 100,
	                                           const Vec3r* camera_center   = nullptr
	                                           );

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
