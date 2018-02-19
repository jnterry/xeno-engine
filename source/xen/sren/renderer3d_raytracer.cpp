////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of renderer3d raytracer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_RAYTRACER_CPP
#define XEN_GRAPHICS_SREN_RENDERER3D_RAYTRACER_CPP

#include <limits>
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

#include "renderer3d.hxx"

#include <cstring>
#include <cstdlib>

namespace xen {
	namespace sren {
		void renderRaytrace (RenderTarget& target,
		                     const xen::Aabb2u& viewport,
		                     const Camera3d& camera,
		                     RenderCommand3d* commands, u32 command_count){

			// :TODO:COMP: view region calc duplicated with rasterizer
			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
			xen::Aabb2u screen_rect = { Vec2u::Origin, target.size - Vec2u{1,1} };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

			Vec2s target_size = (Vec2s)xen::getSize(view_region);

			Angle fov_y = camera.fov_y;
			Angle fov_x = camera.fov_y * ((real)target_size.y / (real)target_size.x);


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
			float image_plane_pixel_offset_x_distance = xen::tan(fov_x / (real)target_size.x) * camera.z_near;
			float image_plane_pixel_offset_y_distance = xen::tan(fov_y / (real)target_size.y) * camera.z_near;

			Vec3r image_plane_center = camera.position - camera.look_dir * camera.z_near;

			Vec3r image_plane_pixel_offset_x =
				xen::normalized(
				                xen::cross(camera.up_dir, camera.look_dir)
				                ) * image_plane_pixel_offset_x_distance;

			Vec3r image_plane_pixel_offset_y =
				-xen::normalized(
				                 xen::cross(image_plane_pixel_offset_x, camera.look_dir)
				                 ) * image_plane_pixel_offset_y_distance;

			Vec2s target_pos;
			for(target_pos.x = 0; target_pos.x < target_size.x; ++target_pos.x) {
				for(target_pos.y = 0; target_pos.y < target_size.y; ++target_pos.y) {

					Vec2r center_offset = (Vec2r)target_pos - ((Vec2r)target_size / 2.0_r);

					// Compute where the ray would intersect the image plane
					Vec3r image_plane_position =
						image_plane_center +
						center_offset.x * image_plane_pixel_offset_x +
						center_offset.y * image_plane_pixel_offset_y;

					Ray3r primary_ray;
					primary_ray.origin    = camera.position;
					primary_ray.direction = xen::normalized(camera.position - image_plane_position);

					Vec3r closest_intersection = Vec3r::Origin;
					real closest_intersection_length = std::numeric_limits<real>::max();
					xen::Color closest_intersection_color;
					bool found_intersection    = false;

					// Do ray cast
					for(u32 cmd_index = 0; cmd_index < command_count; ++cmd_index){
						RenderCommand3d* cmd = &commands[cmd_index];
						if(cmd->type != RenderCommand3d::TRIANGLES){
							continue;
						}

						Mat4r mat_model_inv = xen::getInverse(cmd->model_matrix);

						Ray3r primary_ray_model_space = xen::getTransformed(primary_ray, mat_model_inv);

						//printf("Rendering a triangle mesh with: %i triangles\n", cmd->verticies.count / 3);

						const Triangle3r* tri;

						for(u32 i = 0; i < cmd->verticies.count; i += 3){
							tri = (const Triangle3r*)&cmd->verticies.verticies[i];

							Vec3r intersection;
							real intersection_length;

							if(xen::getIntersection(primary_ray_model_space, *tri, intersection)){
								intersection_length = distanceSq(camera.position, intersection);
								if(closest_intersection_length > intersection_length){
									closest_intersection_length = intersection_length;
									closest_intersection = intersection;
									closest_intersection_color = cmd->color;
								}
								found_intersection = true;
							}
						}
					}
					if(found_intersection){
						// :TODO: target_size.y - target_pos.y is a hack because everything is reflected in y currently
						Vec2s pixel_coord { target_pos.x, target_size.y - target_pos.y };
						pixel_coord += (Vec2s)view_region.min;
						target[pixel_coord.x][pixel_coord.y] = closest_intersection_color;
					}
				}
			}
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Draws debug view of a camera (eg, origin, up dir, look dir, etc),
		/// from some other camera's perspective
		/// \param target          The render target to draw to
		/// \param viewport        The area of the target to draw to
		/// \param camera          The camera to use as the perspective to draw from
		/// \param debugged_camera The camera to draw
		/////////////////////////////////////////////////////////////////////
		void renderCameraDebug(RenderTarget& target, const xen::Aabb2u& viewport,
		                       const Camera3d& view_camera,
		                       const Camera3d& camera
		                       ) {


			LineSegment3r camera_primary_axis = { camera.position,
			                                      camera.position + camera.look_dir * xen::length(camera.position)
			};

			LineSegment3r camera_up_dir = { camera.position,
			                                camera.position + camera.up_dir * 50_r
			};

			Vec3r camera_corner_rays[] = {
				camera.position, camera.position,
				camera.position, camera.position,
				camera.position, camera.position,
				camera.position, camera.position
			};


			////////////////////////////////////////////////////////////////////////////////////////

			// :TODO:COMP: view region calc duplicated with rasterizer
			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
			xen::Aabb2u screen_rect = { Vec2u::Origin, target.size - Vec2u{1,1} };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

			Vec2s target_size = (Vec2s)xen::getSize(view_region);

			Angle fov_y = camera.fov_y;
			Angle fov_x = camera.fov_y * ((real)target_size.y / (real)target_size.x);

			// Compute the image plane center, and offset between each pixel
			// Start with doing this in camera space (so easy conceptually),
			// then transform into world space by lining up z axis with
			// camera's look_dir
			Vec3r image_plane_center = camera.position - camera.look_dir * camera.z_near;

			Vec3r image_plane_pixel_offset_x =
				-xen::normalized(
				                 xen::cross(camera.up_dir, camera.look_dir)
				                 ) * xen::tan(fov_x / (real)target_size.x) * camera.z_near;

			Vec3r image_plane_pixel_offset_y =
				-xen::normalized(
				                 xen::cross(image_plane_pixel_offset_x, camera.look_dir)
				                 ) * xen::tan(fov_y / (real)target_size.y) * camera.z_near;

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
					primary_ray.direction = xen::normalized(camera.position - image_plane_position);

					camera_corner_rays[ray_index*2 + 1] += primary_ray.direction * 100_r;

					++ray_index;
				}
			}

			////////////////////////////////////////////////////////////////////////////////////////

			xen::RenderCommand3d render_commands[3];
			render_commands[0].type                = xen::RenderCommand3d::LINES;
			render_commands[0].color               = xen::Color::MAGENTA;
			render_commands[0].model_matrix        = Mat4r::Identity;
			// :TODO:COMP:ISSUE_5: nasty hack, make line segment an array of row vectors
			render_commands[0].verticies.verticies = &camera_primary_axis.p1;
			render_commands[0].verticies.count     = 2;

			render_commands[1].type                = xen::RenderCommand3d::LINES;
			render_commands[1].color               = xen::Color::GREEN;
			render_commands[1].model_matrix        = Mat4r::Identity;
			// :TODO:COMP:ISSUE_5: nasty hack, make line segment an array of row vectors
			render_commands[1].verticies.verticies = &camera_up_dir.p1;
			render_commands[1].verticies.count     = 2;

			render_commands[2].type                = xen::RenderCommand3d::LINES;
			render_commands[2].color               = xen::Color::WHITE;
			render_commands[2].model_matrix        = Mat4r::Identity;
			// :TODO:COMP:ISSUE_5: nasty hack, make line segment an array of row vectors
			render_commands[2].verticies.verticies = &camera_corner_rays[0];
			render_commands[2].verticies.count     = 8;

			xen::sren::renderRasterize(target, viewport,
			                           view_camera,
			                           render_commands, XenArrayLength(render_commands)
			                           );
		}
	}
}

#endif
