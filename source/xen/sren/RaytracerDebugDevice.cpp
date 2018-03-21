////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Raytracer debug graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEBUGDEVICE_CPP
#define XEN_SREN_RAYTRACERDEBUGDEVICE_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/sren/SoftwareDevice.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/intrinsics.hpp>

#include "SoftwareDeviceBase.hxx"
#include "renderer3d.hxx"
#include "RenderTargetImpl.hxx"

class RaytracerDebugDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::Camera3d         camera_x;
	xen::Camera3d         camera_y;
	xen::Camera3d         camera_z;
	xen::Aabb2u           viewport_main;
	xen::Aabb2u           viewport_x;
	xen::Aabb2u           viewport_y;
	xen::Aabb2u           viewport_z;

	void updateViewports(xen::Aabb2u viewport_whole){
		u32 viewport_padding  = 10;

		Vec2u viewport_whole_size   = viewport_whole.max - viewport_whole.min;
		Vec2u viewport_size   = {
			(viewport_whole_size.x - viewport_padding * 3 ) / 2,
			(viewport_whole_size.y - viewport_padding * 3 ) / 2
		};

		u32 raytrace_size = xen::min<u32>(128, viewport_size.x, viewport_size.y);

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
public:
	~RaytracerDebugDevice(){

	}

	RaytracerDebugDevice()
		: SoftwareDeviceBase(xen::Array<xen::sren::PostProcessor>::EmptyArray) {
		camera_x.position = {300, 0, 0};
		camera_x.look_dir = -Vec3r::UnitX;
		camera_x.up_dir   =  Vec3r::UnitY;


		camera_y.position = {0, 300, 0};
		camera_y.look_dir = -Vec3r::UnitY;
		camera_y.up_dir   =  Vec3r::UnitX;


		camera_z.position = {0, 0, 300};
		camera_z.look_dir = -Vec3r::UnitZ;
		camera_z.up_dir   =  Vec3r::UnitY;
	}

	xen::Mesh createMesh(const xen::MeshData& mesh_data) override {
		// :TODO: implement
		return xen::makeNullHandle<xen::Mesh>();
	}

	void destroyMesh(xen::Mesh mesh) override {
		// :TODO: implement
	}

	void render(xen::RenderTarget render_target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		updateViewports(viewport);
		xen::sren::RenderTargetImpl& target = *this->getRenderTargetImpl(render_target);

		xen::sren::clear(target, viewport, xen::Color::WHITE);

		xen::sren::clear(target, viewport_main, xen::Color::BLACK);

		xen::sren::renderRasterize(target, viewport_main, params, commands);

		xen::sren::renderRaytrace(target, viewport_main, params, commands);

		xen::RenderParameters3d my_params = params;

		camera_x.z_near   = params.camera.z_near;
		camera_x.z_far    = params.camera.z_far;
		camera_x.fov_y    = params.camera.fov_y;
		my_params.camera = camera_x;

		xen::sren::clear(target, viewport_x, xen::Color::BLACK);
		xen::sren::renderRasterize(target, viewport_x, my_params, commands);
		xen::sren::renderCameraDebug(target, viewport_x, camera_x, params.camera);

		camera_y.z_near   = params.camera.z_near;
		camera_y.z_far    = params.camera.z_far;
		camera_y.fov_y    = params.camera.fov_y;
		my_params.camera = camera_y;

		xen::sren::clear(target, viewport_y, xen::Color::BLACK);
		xen::sren::renderRasterize(target, viewport_y, my_params, commands);
		xen::sren::renderCameraDebug(target, viewport_y, camera_y, params.camera);

		camera_z.z_near   = params.camera.z_near;
		camera_z.z_far    = params.camera.z_far;
		camera_z.fov_y    = params.camera.fov_y;
		my_params.camera = camera_z;

		xen::sren::clear(target, viewport_z, xen::Color::BLACK);
		xen::sren::renderRasterize(target, viewport_z, my_params, commands);
		xen::sren::renderCameraDebug(target, viewport_z, camera_z, params.camera);
	}
};

namespace xen {
	GraphicsDevice* createRaytracerDebugDevice(ArenaLinear& arena){
		return xen::emplace<RaytracerDebugDevice>(arena);
	}
}

#endif
