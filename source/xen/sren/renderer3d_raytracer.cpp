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
#include "RenderTargetImpl.hxx"

#include <cstring>
#include <cstdlib>


namespace {

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents the results from casting a ray out into the
	/// scene to be rendered
	/////////////////////////////////////////////////////////////////////
	struct SceneRayCastResult {
		/// \brief The position of the intersection in world space
		Vec3r intersection;

		/// \brief The index of the object with which the intersection occurred
		u32 object_index;

		/// \brief Which triangle of the target object the ray intersected
		u32 triangle_index;
	};

  bool castRayIntoScene(const xen::Ray3r& ray,
	                      const xen::Array<xen::RenderCommand3d>& commands,
	                      SceneRayCastResult& result){

		real closest_intersection_length_sq = std::numeric_limits<real>::max();
		bool found_intersection = false;

		// Loop over all objects in scene
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d* cmd = &commands[cmd_index];
			if(cmd->primative_type != xen::PrimativeType::TRIANGLES){
				continue;
			}

			// :TODO:OPT: we recompute the inverse model matrix for every ray we cast
			// into the scene -> matrix inverse isn't cheap -> cache on per command
			// basis
			Mat4r mat_model_inv = xen::getInverse(cmd->model_matrix);

			xen::Ray3r ray_model_space = xen::getTransformed(ray, mat_model_inv);

			const xen::Triangle3r* tri;

			Vec3r intersection;
			real intersection_length_sq;

			// Loop over all triangles of object
			for(u32 i = 0; i < cmd->immediate.vertex_count; i += 3){
				tri = (const xen::Triangle3r*)&cmd->immediate.position[i];

				if(xen::getIntersection(ray_model_space, *tri, intersection)){
					intersection_length_sq = distanceSq(ray.origin, intersection);
					if(intersection_length_sq < closest_intersection_length_sq){
						closest_intersection_length_sq = intersection_length_sq;

						result.intersection   = intersection * cmd->model_matrix;
						result.object_index   = cmd_index;
						result.triangle_index = i/3;
					  found_intersection = true;
					}
				}
			}
		}

		return found_intersection;
	}
}

namespace xen {
	namespace sren {
		void renderRaytrace (xen::sren::RenderTargetImpl& target,
		                     const xen::Aabb2u& viewport,
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands){

			xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
			Vec2s       target_size = (Vec2s)xen::getSize(view_region);

			Angle fov_y = params.camera.fov_y;
			Angle fov_x = params.camera.fov_y * ((real)target_size.y / (real)target_size.x);

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
			Vec3r image_plane_center = params.camera.position
				                       + params.camera.look_dir * params.camera.z_near;
			Vec3r image_plane_pixel_offset_x =
				xen::normalized(
				                xen::cross(params.camera.up_dir, params.camera.look_dir)
				                ) * xen::tan(fov_x / (real)target_size.x) * params.camera.z_near;
			Vec3r image_plane_pixel_offset_y =
				-xen::normalized(
				                 xen::cross(image_plane_pixel_offset_x, params.camera.look_dir)
				                 ) * xen::tan(fov_y / (real)target_size.y) * params.camera.z_near;

			//////////////////////////////////////////////////////////////////////////
			// Loop over all pixels
			Vec2s target_pos;
			SceneRayCastResult intersection;
			SceneRayCastResult shadow_intersection;
			for(target_pos.x = 0; target_pos.x < target_size.x; ++target_pos.x) {
				for(target_pos.y = 0; target_pos.y < target_size.y; ++target_pos.y) {
					/////////////////////////////////////////////////////////////////////
					// Compute where the ray would intersect the image plane
					Vec2r center_offset = ((Vec2r)target_size / 2.0_r) - (Vec2r)target_pos;
					Vec3r image_plane_position =
						image_plane_center +
						center_offset.x * image_plane_pixel_offset_x +
						center_offset.y * image_plane_pixel_offset_y;

					/////////////////////////////////////////////////////////////////////
					// Construct the primary ray
					Ray3r primary_ray;
					primary_ray.origin    = image_plane_position;;
					primary_ray.direction = xen::normalized(params.camera.position - image_plane_position);

					/////////////////////////////////////////////////////////////////////
					// Cast the ray into the scene
					if(!castRayIntoScene(primary_ray, commands, intersection)){
						continue;
					}

					Color4f pixel_color = commands[intersection.object_index].color;
					Color3f total_light = params.ambient_light;

					/////////////////////////////////////////////////////////////////////
					// Cast shadow ray
                    for(u64 i = 0; i < params.lights.size; ++i){
						//printf("%i, %i :::::: Casting shadow ray for light %i\n",
						//       target_pos.x, target_pos.y, i);
						Ray3r shadow_ray;

						switch(params.lights[i].type){
						case xen::LightSource3d::POINT: {
							shadow_ray.origin    = intersection.intersection;
							shadow_ray.direction = xen::normalized(params.lights[i].point.position -
							                                       intersection.intersection
							                                      );

							real light_dist_sq = xen::distanceSq(params.lights[i].point.position,
							                                     intersection.intersection
							                                    );

							if(castRayIntoScene(shadow_ray, commands, shadow_intersection)){
								real geom_dist_sq  = xen::distanceSq(shadow_intersection.intersection,
								                                     intersection.intersection
								                                    );

								if(light_dist_sq > geom_dist_sq) {
									// Then light source if blocked by geometry
									break;
								}
							}

							real attenuation = (params.lights[i].attenuation.x * 1.0+
							                    params.lights[i].attenuation.y * xen::sqrt(light_dist_sq) +
							                    params.lights[i].attenuation.z * light_dist_sq
							                    );

							//printf("Attenuation: %f, distance: %f\n", attenuation, light_dist_sq);

							total_light += (params.lights[i].color / attenuation).rgb * params.lights[i].color.w;

							break;
						}
						default:
							printf("WARN: Unhandled light type in raytracer, type: %i\n",
							       params.lights[i].type);
							break;
						}
					}

					pixel_color.rgb *= total_light;

					/////////////////////////////////////////////////////////////////////
					// Color the pixel
					Vec2s pixel_coord = target_pos + (Vec2s)view_region.min;
					target.color[pixel_coord.y*target.width + pixel_coord.x] = pixel_color;
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
		void renderCameraDebug(xen::sren::RenderTargetImpl& target, const xen::Aabb2u& viewport,
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

			//////////////////////////////////////////////////////////////////////////

			// :TODO:COMP: view region calc duplicated with rasterizer
			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
            xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
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
			Vec3r image_plane_center = camera.position + camera.look_dir * camera.z_near;
			Vec3r image_plane_pixel_offset_x =
				xen::normalized(
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

			//////////////////////////////////////////////////////////////////////////

			xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
			render_commands[0].primative_type         = xen::PrimativeType::LINES;
			render_commands[0].color                  = xen::Color::MAGENTA4f;
			render_commands[0].model_matrix           = Mat4r::Identity;
			render_commands[0].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[0].immediate.position     = &camera_primary_axis.vertices[0];
			render_commands[0].immediate.vertex_count = 2;

		  render_commands[1].primative_type         = xen::PrimativeType::LINES;
			render_commands[1].color                  = xen::Color::GREEN4f;
			render_commands[1].model_matrix           = Mat4r::Identity;
			render_commands[1].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[1].immediate.position     = &camera_up_dir.vertices[0];
			render_commands[1].immediate.vertex_count = 2;

			render_commands[2].primative_type         = xen::PrimativeType::LINES;
			render_commands[2].color                  = xen::Color::WHITE4f;
			render_commands[2].model_matrix           = Mat4r::Identity;
			render_commands[2].geometry_source        = xen::RenderCommand3d::IMMEDIATE;
			render_commands[2].immediate.position     = &camera_corner_rays[0];
			render_commands[2].immediate.vertex_count = 8;

			xen::RenderParameters3d params = {};
			params.camera = view_camera;

			xen::sren::renderRasterize(target, viewport, params, render_commands);
		}
	}
}

#endif
