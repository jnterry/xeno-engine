////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Raytracer debug graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEBUGDEVICE_CPP
#define XEN_SREN_RAYTRACERDEBUGDEVICE_CPP

#include "RaytracerDevice.hxx"
#include "rasterizer3d.hxx"
#include "raytracer3d.hxx"
#include "RenderTargetImpl.hxx"
#include "render-utilities.hxx"

#include <xen/sren/SoftwareDevice.hpp>
#include <xen/sren/FragmentShader.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>

class RaytracerDebugDevice : public xen::sren::RaytracerDevice {
private:
	xen::Camera3d         camera_x;
	xen::Camera3d         camera_y;
	xen::Camera3d         camera_z;
	xen::Aabb2u           viewport_main;
	xen::Aabb2u           viewport_x;
	xen::Aabb2u           viewport_y;
	xen::Aabb2u           viewport_z;

	///< \brief Distance along each axis that debug cameras should be placed at
	///         from the debug_camera_center
	real                  debug_camera_distance;

	/// \brief Center point that debug cameras should look at. If set to nullptr
	///        then position of the main render camera will be used as center
	const Vec3r*          debug_camera_center;

	void updateViewports(xen::Aabb2u viewport_whole){
		u32 viewport_padding  = 10;

		Vec2u viewport_whole_size   = viewport_whole.max - viewport_whole.min;
		Vec2u viewport_size   = {
			(viewport_whole_size.x - viewport_padding * 3 ) / 2,
			(viewport_whole_size.y - viewport_padding * 3 ) / 2
		};

		u32 raytrace_size = xen::min<u32>(300, viewport_size.x, viewport_size.y);

		viewport_main = xen::makeAabbFromMinAndSize
			(
			 viewport_padding + (viewport_size.x - raytrace_size)/2,
			 viewport_padding + (viewport_size.y - raytrace_size)/2,
			 raytrace_size, raytrace_size
			);

		viewport_y = xen::makeAabbFromMinAndSize
			(
			 viewport_padding * 2 + viewport_size.x,
			 viewport_padding,
			 viewport_size.x, viewport_size.y
			);

		viewport_x = xen::makeAabbFromMinAndSize
			(
			 viewport_padding * 1 + 0,
			 viewport_padding * 2 + viewport_size.y,
			 viewport_size.x, viewport_size.y
			);

		viewport_z = xen::makeAabbFromMinAndSize
			(
			 viewport_padding * 2 + viewport_size.x,
			 viewport_padding * 2 + viewport_size.y,
			 viewport_size.x, viewport_size.y
			);

		viewport_main.min += viewport_whole.min;
		viewport_x.min    += viewport_whole.min;
		viewport_y.min    += viewport_whole.min;
		viewport_z.min    += viewport_whole.min;
	}

	void renderDebugView(xsren::RenderTarget&           target,
	                     const xen::Aabb2u&                     viewport,
	                     const xen::RenderParameters3d&         params,
	                     const xen::Array<xen::RenderCommand3d> commands,
	                     xen::Camera3d                          view_camera){

		xen::Aabb2r viewport_r = xen::cast<xen::Aabb2r>(viewport);

		view_camera.z_near   = params.camera.z_near;
		view_camera.z_far    = params.camera.z_far;
		view_camera.fov_y    = params.camera.fov_y;

		xen::RenderParameters3d my_params = params;
		my_params.camera = view_camera;

		Mat4r vp_matrix = xen::getViewProjectionMatrix(view_camera, viewport);

		xen::sren::RasterizationContext context;
		context.target          = &target;
		context.viewport        = &viewport_r;
		context.fragment_shader = xen::sren::FragmentShader_Default;
		context.vp_matrix       = vp_matrix;
		context.m_matrix        = Mat4r::Identity;
		*(xen::RenderParameters3d*)(&context) = params;
		context.textures[0]     = nullptr;
		context.textures[1]     = nullptr;
		context.textures[2]     = nullptr;
		context.textures[3]     = nullptr;
		context.ambient_light   = xen::Color::WHITE4f.rgb;

		/////////////////////////////////////////////////
		// Render the scene geometry
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			context.diffuse_color      = commands[cmd_index].color;
			context.emissive_color     = commands[cmd_index].emissive_color;
			context.specular_exponent  = commands[cmd_index].specular_exponent;
			context.specular_intensity = commands[cmd_index].specular_intensity;
			rasterizeMesh(context,
			              commands[cmd_index].primitive_type,
			              *this->mesh_store.getMesh(commands[cmd_index].mesh));
		}

		/////////////////////////////////////////////////
		// Render geometry for the camera we are debugging
		xen::sren::renderCameraDebug(target, viewport,
		                             view_camera, params.camera,
		                             debug_camera_distance * 0.3_r
		                            );
	}

public:
	~RaytracerDebugDevice(){
		// no-op
	}

	RaytracerDebugDevice(real camera_distance, const Vec3r* camera_center)
		: RaytracerDevice(xen::Array<xen::sren::PostProcessor*>::EmptyArray),
		  debug_camera_distance(camera_distance),
		  debug_camera_center  (camera_center)
	{
		camera_x.position = {0, 0, 0};
		camera_x.look_dir = -Vec3r::UnitX;
		camera_x.up_dir   =  Vec3r::UnitY;

		camera_y.position = {0, 0, 0};
		camera_y.look_dir = -Vec3r::UnitY;
		camera_y.up_dir   =  Vec3r::UnitX;

		camera_z.position = {0, 0, 0};
		camera_z.look_dir = -Vec3r::UnitZ;
		camera_z.up_dir   =  Vec3r::UnitY;
	}

	virtual void doRender(xsren::RenderTarget&           target,
	                      const xen::Aabb2u&                     viewport,
	                      const xen::RenderParameters3d&         params,
	                      const xen::Array<xen::RenderCommand3d> commands,
	                      const xen::Array<u32>                  non_triangle_cmds,
	                      const xen::sren::RaytracerScene&       scene){

		////////////////////////////////////////////////////////////////////////////
		// Update viewport and cameras
		updateViewports(viewport);

		Vec3r center_point;
		if(this->debug_camera_center == nullptr){
			center_point = params.camera.position;
		} else {
			center_point = *this->debug_camera_center;
		}
		camera_x.position = center_point + Vec3r::UnitX * debug_camera_distance;
		camera_y.position = center_point + Vec3r::UnitY * debug_camera_distance;
		camera_z.position = center_point + Vec3r::UnitZ * debug_camera_distance;
		////////////////////////////////////////////////////////////////////////////

		xsren::clear(target, viewport,      xen::Color::WHITE);
		xsren::clear(target, viewport_main, xen::Color::BLACK);
		xsren::clear(target, viewport_x,    xen::Color::BLACK);
		xsren::clear(target, viewport_y,    xen::Color::BLACK);
		xsren::clear(target, viewport_z,    xen::Color::BLACK);

		/////////////////////////////////////////////////////////
		// Render the raytraced portion to the main view
		RaytracerDevice::doRender(target, viewport_main, params,
		                          commands, non_triangle_cmds,
		                          scene);

		/////////////////////////////////////////////////////////
		// Render the debug cameras, using rasterizer for quicker rendering
		renderDebugView(target, viewport_x, params, commands, camera_x);
		renderDebugView(target, viewport_y, params, commands, camera_y);
		renderDebugView(target, viewport_z, params, commands, camera_z);
	}
};

namespace xen {
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena,
	                                           real         camera_distance,
	                                           const Vec3r* camera_center
	                                          ){
		return xen::emplace<RaytracerDebugDevice>(arena,
		                                          camera_distance, camera_center
		                                         );
	}
}

#endif
