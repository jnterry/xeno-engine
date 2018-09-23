////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation functions for rendering debug visualisations
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERDEBUG_CPP
#define XEN_SREN_RENDERDEBUG_CPP

#include <xen/graphics/Color.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array.hpp>

#include <xen/sren/FragmentShader.hpp>
#include <xen/sren/RenderTarget.hxx>
#include <xen/sren/render-debug.hxx>
#include <xen/sren/rasterizer3d.hxx>

void xsr::renderCameraDebug(xsr::RenderTarget& target,
                              const xen::Aabb2u&   viewport,
                              const xen::Camera3d& view_camera,
                              const xen::Camera3d& camera,
                              real                 camera_scale
                              ) {

	Vec3r camera_local_axes[] = {
		camera.position, camera.position,
		camera.position, camera.position,
		camera.position, camera.position,
	};

	xen::Color camera_local_axes_colors[] = {
		xen::Color::RED,   xen::Color::RED,
		xen::Color::GREEN, xen::Color::GREEN,
		xen::Color::BLUE,  xen::Color::BLUE
	};

	Vec3r camera_corner_rays[] = {
		camera.position, camera.position,
		camera.position, camera.position,
		camera.position, camera.position,
		camera.position, camera.position
	};

	//////////////////////////////////////////////////////////////////////////

	// :TODO:COMP: view region calc duplicated with rasterizer
	// Find the actual view_region we wish to draw to. This is the
	// intersection of the actual target, and the user specified viewport
	xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
	xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

	Vec2s target_size = (Vec2s)xen::getSize(view_region);

	xen::Angle fov_y = camera.fov_y;
	xen::Angle fov_x = camera.fov_y * ((real)target_size.y / (real)target_size.x);

	// Compute the local axes of the camera
	Vec3r cam_zaxis = camera.look_dir;
	Vec3r cam_xaxis = xen::cross(camera.look_dir, camera.up_dir);
	Vec3r cam_yaxis = xen::cross(cam_xaxis, cam_zaxis);

	// Compute distance between pixels on the image plane in world space using
	// a bit of trig
	//            x
	//         _______
	//         |     /
	//         |    /
	//  z_near |   /
	//         |  /
	//         | /
	//         |/ angle = fov_x / target_width
	Vec3r image_plane_center = (camera.position +
	                            cam_zaxis * camera.z_near
	                            );

	// In a typical scene the camera is usually at a +ve z position looking in
	// the -ve z direction.
	// In camera space however we assume that the camera is looking down its
	// own local z axis, IE, in a +ve z direction. This means that the
	// camera's z axis and world's z axis are pointing in opposite directions.
	// This cannot be encoded as a rotation without at least one of the x
	// and y axes also not lining up with their world space counter parts.
	// This would be inconvenient when drawing to the screen, as either x or
	// y would be reversed.
	// Instead we encode this flip as a conversion between a right handed
	// coordinate system in world space, but left handed in camera space.
	// This is exactly the same as a mirror which really inverts the z axis -
	// hence why the image is flipped horizontally but not vertically.
	// It is for this reason we introduce a minus sign at the start of the
	// line below; we must invert one of the axes in order to convert from the
	// right handed world space to the left handed camera space.
	// Note that OpenGL does this too - world space is right handed, camera
	// space is left handed. This is precisely so the x and y axes of both
	// spaces are in the same direction, while the z axis can point in the
	// opposite direction.
	Vec3r image_plane_pixel_offset_x = -(xen::normalized(cam_xaxis) *
	                                     xen::tan(fov_x / (real)target_size.x) * camera.z_near
	                                     );
	Vec3r image_plane_pixel_offset_y = (xen::normalized(cam_yaxis) *
	                                    xen::tan(fov_y / (real)target_size.y) * camera.z_near
	                                    );

	camera_local_axes[1] += cam_xaxis * camera_scale * 0.6_r;
	camera_local_axes[3] += cam_yaxis * camera_scale * 0.6_r;
	camera_local_axes[5] += cam_zaxis * camera_scale * 0.6_r;

	Vec2s target_pos;
	int ray_index = 0;

	for(target_pos.x = 0; target_pos.x < target_size.x; target_pos.x += target_size.x-1) {
		for(target_pos.y = 0; target_pos.y < target_size.y; target_pos.y += target_size.y-1) {

			Vec2r center_offset = (Vec2r)target_pos - ((Vec2r)target_size / 2.0_r);

			// Compute where the ray would intersect the image plane
			Vec3r image_plane_position =
				image_plane_center +
				center_offset.x * image_plane_pixel_offset_x +
				center_offset.y * image_plane_pixel_offset_y;

			xen::Ray3r primary_ray;
			primary_ray.origin    = camera.position;
			primary_ray.direction = xen::normalized(image_plane_position - camera.position);

			camera_corner_rays[ray_index*2 + 1] += primary_ray.direction * camera_scale;

			++ray_index;
		}
	}

	//////////////////////////////////////////////////////////////////////////

	Mat4r vp_matrix = xen::getViewProjectionMatrix(view_camera, viewport);

	xsr::RasterizationContext context;
	xen::clearToZero(&context);
	context.camera          = view_camera;
	context.target          = &target;
	context.fragment_shader = xsr::FragmentShader_Default;
	context.viewport        = &view_region;
	context.m_matrix        = Mat4r::Identity;
	context.vp_matrix       = vp_matrix;
	context.diffuse_color   = xen::Color::WHITE4f;
	context.emissive_color  = xen::Color::WHITE4f;
	context.textures[0]     = nullptr;
	context.textures[1]     = nullptr;
	context.textures[2]     = nullptr;
	context.textures[3]     = nullptr;

	xsr::rasterizeLinesModel(context,
	                           camera_local_axes,
	                           camera_local_axes_colors,
	                           XenArrayLength(camera_local_axes));

	xsr::rasterizeLinesModel(context,
	                           camera_corner_rays,
	                           nullptr, // color buffer
	                           XenArrayLength(camera_corner_rays));
}

void xsr::renderDebugBoundingBox(xsr::RenderTarget& target,
                                   const xen::Aabb2u&   viewport,
                                   const xen::Camera3d& camera,
                                   xen::Aabb3r          aabb,
                                   xen::Color4f         color){

	xen::Aabb2r viewport_r = xen::cast<xen::Aabb2r>(viewport);

	xsr::RasterizationContext cntx;
	xen::clearToZero(&cntx);
	cntx.target          = &target;
	cntx.viewport        = &viewport_r; // :TODO: why is this a pointer... :(
	cntx.vp_matrix       = xen::getViewProjectionMatrix(camera, viewport);
	cntx.fragment_shader = xsr::FragmentShader_Default;
	cntx.diffuse_color   = color;
	cntx.emissive_color  = xen::Color::WHITE4f;
	cntx.m_matrix        = (xen::Scale3d      (xen::getSize(aabb)) *
	                        xen::Translation3d(aabb.min          )
	                       );
	rasterizeLinesModel(cntx,
	                    xen::TestMeshGeometry_UnitCubeLines.position,
	                    xen::TestMeshGeometry_UnitCubeLines.color,
	                    xen::TestMeshGeometry_UnitCubeLines.vertex_count,
	                    2); //advance by 2 vertex for each line drawn

}

#endif
