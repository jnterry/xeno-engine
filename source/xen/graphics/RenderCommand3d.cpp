////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of functions defined in RenderCommand3d.hpp
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_RENDERCOMMAND3D_CPP
#define XEN_GRAPHICS_RENDERCOMMAND3D_CPP

#include <xen/graphics/RenderCommand3d.hpp>

namespace xen{

	RenderParameters3d::RenderParameters3d()
		: ambient_light(Color3f{1,1,1}), lights({0, nullptr}){
		camera.fov_y    = 70_deg;
		camera.z_near   = 0.001_r;
		camera.z_far    = 1000_r;
		camera.position =  Vec3r::UnitZ * 10_r;
		camera.look_dir = -Vec3r::UnitZ;
		camera.up_dir   =  Vec3r::UnitY;
	}

}

#endif
