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
#include <xen/core/array.hpp>

#include "raytracer3d.hxx"
#include "render-utilities.hxx"
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
		Vec3r pos_world;

		/// \brief The position of the intersection in model space
		Vec3r pos_model;

		/// \brief The square of the distance between the ray's origin and
		/// the intersection position in world space
	  real dist_sq;

		/// \brief The index of the object with which the intersection occurred
		u32 cmd_index;

		/// \brief Which triangle of the target object the ray intersected
		u32 tri_index;
	};

  bool castRayIntoScene(const xen::Ray3r& ray_world,
	                      const xen::Array<xen::RenderCommand3d>& commands,
	                      SceneRayCastResult& result){

		result.dist_sq = std::numeric_limits<real>::max();
		bool found_intersection = false;

		// Loop over all objects in scene
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d* cmd = &commands[cmd_index];
			if(cmd->primitive_type != xen::PrimitiveType::TRIANGLES){
				continue;
			}

			// :TODO:OPT: we recompute the inverse model matrix for every ray we cast
			// into the scene -> matrix inverse isn't cheap -> cache on per command
			// basis
			Mat4r mat_model_inv = xen::getInverse(cmd->model_matrix);

			// Compute the ray in model space.
			// This is faster (2 vertices to define a ray) than transforming the mesh
			// into world space (arbitrary number of vertices)
			xen::Ray3r ray_model = xen::getTransformed(ray_world, mat_model_inv);

			const xen::Triangle3r* tri;

			Vec3r intersection_model;
			Vec3r intersection_world;
			real  intersection_length_sq;

			// Loop over all triangles of object
			for(u32 i = 0; i < cmd->immediate.vertex_count; i += 3){
				tri = (const xen::Triangle3r*)&cmd->immediate.position[i];

				if(!xen::getIntersection(ray_model, *tri, intersection_model)){
					// Then the ray does not intersection this triangle
					continue;
				}

				intersection_world = intersection_model * cmd->model_matrix;
				intersection_length_sq = xen::distanceSq(ray_world.origin, intersection_world);

				if(intersection_length_sq >= result.dist_sq){
					// Then we've already found a closer intersection. Ignore this one
					continue;
				}

				result.dist_sq     = intersection_length_sq;
				result.pos_world   = intersection_world;
				result.pos_model   = intersection_model;
				result.cmd_index   = cmd_index;
				result.tri_index   = i/3;
				found_intersection = true;
			}
		}

		return found_intersection;
	}
}

namespace xen {
	namespace sren {
		void renderRaytrace (xen::sren::RenderTargetImpl&       target,
		                     const xen::Aabb2u&                 viewport,
		                     const RenderParameters3d&          params,
		                     const xen::Array<RenderCommand3d>& commands){

			xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
			Vec2s       target_size = (Vec2s)xen::getSize(view_region);

			Angle fov_y = params.camera.fov_y;
			Angle fov_x = params.camera.fov_y * ((real)target_size.y / (real)target_size.x);

			// Compute the local axes of the camera
			Vec3r cam_zaxis = params.camera.look_dir;
			Vec3r cam_xaxis = xen::cross(params.camera.look_dir, params.camera.up_dir);
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
			Vec3r image_plane_center = (params.camera.position +
			                            cam_zaxis * params.camera.z_near
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
			                                     xen::tan(fov_x / (real)target_size.x) * params.camera.z_near
			                                    );
			Vec3r image_plane_pixel_offset_y = (xen::normalized(cam_yaxis) *
			                                    xen::tan(fov_y / (real)target_size.y) * params.camera.z_near
			                                   );

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
					primary_ray.direction = xen::normalized(image_plane_position - params.camera.position);

					/////////////////////////////////////////////////////////////////////
					// Cast the ray into the scene
					if(!castRayIntoScene(primary_ray, commands, intersection)){
						continue;
					}

					XenDebugAssert(intersection.cmd_index < xen::size(commands),
					               "Expected intersection's object index to be within "
					               "bounds of the command list");
					XenDebugAssert(intersection.tri_index * 3 <
					               commands[intersection.cmd_index].immediate.vertex_count,
					               "Expected intersection's triangle index to be within "
					               "bounds of the vertex list");

					// Compute surface properties at intersection point
					xen::Color4f pixel_color        = xen::Color::WHITE4f;
					Vec3r        pixel_normal_world = Vec3r::Origin;
					{
						Vec3r* pbuf_model = commands[intersection.cmd_index].immediate.position;
						Vec3r* nbuf_model = commands[intersection.cmd_index].immediate.normal;
						Color* cbuf       = commands[intersection.cmd_index].immediate.color;
						u32    buf_index = intersection.tri_index * 3;

						xen::Triangle3r ptri_model = *(xen::Triangle3r*)&pbuf_model[buf_index];

						// Get the barycentric coordinates of the intersection
						Vec3r bary = xen::getBarycentricCoordinates(ptri_model, intersection.pos_model);

						XenDebugAssert(bary.x >= 0, "Expected barycentric x component to be positive");
						XenDebugAssert(bary.y >= 0, "Expected barycentric y component to be positive");
						XenDebugAssert(bary.z >= 0, "Expected barycentric z component to be positive");

						// If we have per vertex color information use it rather than WHITE
						if(cbuf != nullptr){
							// Extract the per vertex attributes for the triangle we have intersected

							xen::Triangle4f ctri;
							ctri.p1 = xen::makeColor4f(cbuf[buf_index + 0]);
							ctri.p2 = xen::makeColor4f(cbuf[buf_index + 1]);
							ctri.p3 = xen::makeColor4f(cbuf[buf_index + 2]);

							pixel_color = evaluateBarycentricCoordinates(ctri, bary);
						}

						Vec3r pixel_normal_model;
						if(nbuf_model != nullptr){
							pixel_normal_model = nbuf_model[buf_index];
						} else {
							// :TODO: lets not support immediate rendering
							// have the device store meshes, and force normal information, since
							// we CANNOT do sensible lighting calculations without it...
							//
							// THIS MEANS WINDING ORDER MATTERS!!!
							pixel_normal_model = xen::computeTriangleNormal(ptri_model);
						}
						pixel_normal_world = xen::normalized(pixel_normal_model *
						                                     commands[intersection.cmd_index].model_matrix
						                                    );
					}

					// compute light hitting surface at intersection point
					Color3f total_light = params.ambient_light;
					{
						/////////////////////////////////////////////////////////////////////
						// Cast shadow ray
						for(u64 i = 0; i < params.lights.size; ++i){
							//printf("%i, %i :::::: Casting shadow ray for light %i\n",
							//       target_pos.x, target_pos.y, i);
							Ray3r shadow_ray;

							switch(params.lights[i].type){
							case xen::LightSource3d::POINT: {
								shadow_ray.origin    = intersection.pos_world;

								// Don't shoot directly from the surface or we may intersect
								// ourselves. Push the origin out slightly
								shadow_ray.origin += pixel_normal_world * 0.001_r;

								shadow_ray.direction = xen::normalized(params.lights[i].point.position -
								                                       intersection.pos_world
								                                       );

								real light_dist_sq = xen::distanceSq(params.lights[i].point.position,
								                                     intersection.pos_world
								                                     );

								if(castRayIntoScene(shadow_ray, commands, shadow_intersection) &&
								   light_dist_sq > shadow_intersection.dist_sq){
									// Then there is geometry between the intersection.pos_world and
									// this light. Hence the light source is blocked
									break;
								}

								total_light +=
									xen::sren::computeLightInfluence(params.lights[i].color,
									                                 params.lights[i].attenuation,
									                                 light_dist_sq);

								break;
							}
							default:
								printf("WARN: Unhandled light type in raytracer, type: %i\n",
								       params.lights[i].type);
								break;
							}
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
	}
}

#endif
