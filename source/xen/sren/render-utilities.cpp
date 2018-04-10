////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of utility functions for the 3d renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERUTILITIES_CPP
#define XEN_SREN_RENDERUTILITIES_CPP

#include <xen/graphics/Color.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/array.hpp>

#include "RenderTargetImpl.hxx"
#include "render-utilities.hxx"
#include "rasterizer3d.hxx"

namespace xen {
	namespace sren {
		void clear(xen::sren::RenderTargetImpl& target, Color color) {
			Color4f color01 = (Color4f)color;
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.color[i] = color01;
			}
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.depth[i] = FLT_MAX;
			}
		}

		void clear(RenderTargetImpl& target, const xen::Aabb2u& viewport, Color color){
			Color4f color01 = (Color4f)color;

			for(u32 y = viewport.min.y; y < viewport.max.y; ++y){
				// still need to stride by width of whole target to get to next line in
				// y, not just the width of the viewport
				u32 base = y * target.width;
				for(u32 x = viewport.min.x; x < viewport.max.x; ++x){
					target.color[base + x] = color01;
					target.depth[base + x] = FLT_MAX;
				}
			}
		}

		xen::Color3f computeLightInfluence(xen::Color4f light_color,
		                                   Vec3f        attenuation_coefficents,
		                                   real         distance_sq){
			float attenuation = (attenuation_coefficents.x * 1.0 +
			                     attenuation_coefficents.y * xen::sqrt(distance_sq) +
			                     attenuation_coefficents.z * distance_sq
			                     );

			return (light_color / attenuation).rgb * light_color.w;
		}

		void renderCameraDebug(xen::sren::RenderTargetImpl& target,
		                       const xen::Aabb2u& viewport,
		                       const Camera3d& view_camera,
		                       const Camera3d& camera,
		                       real            camera_scale
		                       ) {

			// :TODO: not sure how to do this after mesh refactor
			//  -> auto load test meshes and use them?
			//     cube lines transformed by inverse view matrix for camera view frustrum
			//     axes mesh for camera local axes, etc

			#if 0


			LineSegment3r camera_primary_axis = { camera.position,
			                                      camera.position + camera.look_dir * xen::length(camera.position)
			};

			Vec3r camera_local_axes[] = {
				camera.position, camera.position,
				camera.position, camera.position,
				camera.position, camera.position,
			};

			Color camera_local_axes_colors[] = {
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

			Angle fov_y = camera.fov_y;
			Angle fov_x = camera.fov_y * ((real)target_size.y / (real)target_size.x);

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

			camera_local_axes[1] += cam_xaxis * camera_scale * 0.3_r;
			camera_local_axes[3] += cam_yaxis * camera_scale * 0.3_r;
			camera_local_axes[5] += cam_zaxis * camera_scale * 0.3_r;

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

					Ray3r primary_ray;
					primary_ray.origin    = camera.position;
					primary_ray.direction = xen::normalized(image_plane_position - camera.position);

					camera_corner_rays[ray_index*2 + 1] += primary_ray.direction * camera_scale;

					++ray_index;
				}
			}

			//////////////////////////////////////////////////////////////////////////

			xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
			xen::clearToZero(render_commands);

			/*
			render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
			render_commands[0].color                  = xen::Color::MAGENTA4f;
			render_commands[0].model_matrix           = Mat4r::Identity;
			render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[0].immediate.position     = &camera_primary_axis.vertices[0];
			render_commands[0].immediate.vertex_count = 2;

		  render_commands[1].primitive_type         = xen::PrimitiveType::LINES;
			render_commands[1].color                  = xen::Color::WHITE4f;
			render_commands[1].model_matrix           = Mat4r::Identity;
			render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[1].immediate.position     = camera_local_axes;
			render_commands[1].immediate.color        = camera_local_axes_colors;
			render_commands[1].immediate.vertex_count = XenArrayLength(camera_local_axes);

			render_commands[2].primitive_type         = xen::PrimitiveType::LINES;
			render_commands[2].color                  = xen::Color::WHITE4f;
			render_commands[2].model_matrix           = Mat4r::Identity;
			render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[2].immediate.position     = &camera_corner_rays[0];
			render_commands[2].immediate.vertex_count = 8;

			xen::RenderParameters3d params = {};
			params.camera = view_camera;

			xen::sren::renderRasterize(target, viewport, params, render_commands);*/
			#endif
		}
	}
}

#endif