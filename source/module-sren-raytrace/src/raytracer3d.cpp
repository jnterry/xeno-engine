////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of renderer3d raytracer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RAYTRACER3D_CPP
#define XEN_GRAPHICS_SREN_RAYTRACER3D_CPP

#include <limits>
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/core/array.hpp>
#include <xen/kernel/log.hpp>

#include <xen/sren/FragmentShader.hpp>
#include <xen/sren/RenderTarget.hxx>
#include "raytracer3d.hxx"
#include <xen/sren/render-debug.hxx>

#include <cstring>
#include <cstdlib>

namespace xsr {

bool castRayIntoScene(const xen::Ray3r& ray_world,
                      const RaytracerScene& scene,
                      SceneRayCastResult& result,
                      bool skip_non_shadow_casters){

	result.dist_sq_world = std::numeric_limits<real>::max();
	bool found_intersection = false;

	// pre-compute division once per ray -> used for aabb intersection
	// see: https://tavianator.com/fast-branchless-raybounding-box-intersections/
	//
	// note also we don't need to check for divides by 0, since the resulting
	// infinities will still work in the comparison operations later...
	Vec3r ray_world_dir_inv = Vec3r{
		1.0_r / ray_world.direction.x,
		1.0_r / ray_world.direction.y,
		1.0_r / ray_world.direction.z,
	};

	u32 start_index = scene.first_shadow_caster * skip_non_shadow_casters;

	// Loop over all objects in scene
	for(u32 model_index = start_index;
	    model_index < xen::size(scene.models);
	    ++model_index){
		const xsr::RaytracerModel& model = scene.models[model_index];

		////////////////////////////////////////////////////////////////////////////
		// Check if the ray intersects the model's aabb in world space.
		// If not we can immediately skip checking this model
		// See: https://tavianator.com/fast-branchless-raybounding-box-intersections-part-2-nans/
		{
			xen::Aabb3r b = model.aabb_world;
			xen::Ray3r  r = ray_world;

			real t1 = (b.min.x - r.origin.x) * ray_world_dir_inv.x;
			real t2 = (b.max.x - r.origin.x) * ray_world_dir_inv.x;

			real tmin = xen::min(t1, t2);
			real tmax = xen::max(t1, t2);

			for(u32 i = 1; i < 3; ++i){
				t1 = (b.min[i] - r.origin[i]) * ray_world_dir_inv[i];
				t2 = (b.max[i] - r.origin[i]) * ray_world_dir_inv[i];

				tmin = xen::max(tmin, xen::min(t1, t2, tmax));
				tmax = xen::min(tmax, xen::max(t1, t2, tmin));
			}

			if(tmax < xen::max(0.0_r, tmin)){
				continue;
			}
		}

		////////////////////////////////////////////////////////////////////////////

		////////////////////////////////////////////////////////////////////////////
		// Compute the ray in model space
		//
		// This is faster (2 vertices to define a ray) than transforming the mesh
		// into world space (arbitrary number of vertices)
		xen::Ray3r ray_model = xen::getTransformed(ray_world, model.inv_model_matrix);

		const xen::Triangle3r* tri;

		Vec3r intersection_model;
		Vec3r intersection_world;
		real  intersection_length_sq;

		// Loop over all triangles of object
		for(u32 i = 0; i < model.mesh->vertex_count; i += 3){
			tri = (const xen::Triangle3r*)&model.mesh->position[i];

			if(!xen::getIntersection(ray_model, *tri, intersection_model)){
				// Then the ray does not intersection this triangle
				continue;
			}

			intersection_world     = intersection_model * model.model_matrix;
			intersection_length_sq = xen::distanceSq(ray_world.origin, intersection_world);

			if(intersection_length_sq >= result.dist_sq_world){
				// Then we've already found a closer intersection. Ignore this one
				continue;
			}

			result.dist_sq_world = intersection_length_sq;
			result.pos_world     = intersection_world;
			result.pos_model     = intersection_model;
			result.model_index   = model_index;
			result.tri_index     = i/3;
			found_intersection   = true;
		}
	}

	return found_intersection;
} // end of cast ray into scene

void renderRaytrace (xsr::RenderTarget&       target,
                     const xen::Aabb2u&                 viewport,
                     const xen::RenderParameters3d&     params,
                     const RaytracerScene&              scene,
                     const xen::Aabb2u&                 rendering_bounds
                     ){

	xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
	xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
	Vec2s       target_size = (Vec2s)xen::getSize(view_region);

	xen::Angle fov_y = params.camera.fov_y;
	xen::Angle fov_x = params.camera.fov_y * ((real)target_size.y / (real)target_size.x);

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
	//
	// In a typical scene the camera is usually at a +ve z position looking in
	// the -ve z direction. In camera space however we assume that the camera is
	// looking down its own local z axis, IE, in a +ve z direction. Flipping 1
	// axis without flipping others is a change from right-handed world space
	// top left-handed camera space, hence the - in front of x but not y
	//
	// see: notes.terry.cloud/cs/graphics/handedness-cameras-and-mirrors
	// for details
	Vec3r image_plane_center = (params.camera.position +
	                            cam_zaxis * params.camera.z_near
	                           );
	Vec3r image_plane_pixel_offset_x = -(xen::normalized(cam_xaxis) *
	                                     xen::tan(fov_x / (real)target_size.x) * params.camera.z_near
	                                     );
	Vec3r image_plane_pixel_offset_y = (xen::normalized(cam_yaxis) *
	                                    xen::tan(fov_y / (real)target_size.y) * params.camera.z_near
	                                    );

	//////////////////////////////////////////////////////////////////////////
	// Loop over all pixels
	Vec2u target_pos;
	SceneRayCastResult intersection;
	SceneRayCastResult shadow_intersection;
	for(target_pos.x = rendering_bounds.min.x;
	    target_pos.x < rendering_bounds.max.x;
	    ++target_pos.x) {
		for(target_pos.y = rendering_bounds.min.y;
		    target_pos.y < rendering_bounds.max.y;
		    ++target_pos.y) {
			/////////////////////////////////////////////////////////////////////
			// Compute where the ray would intersect the image plane
			Vec2r center_offset = (((Vec2r)target_size / 2.0_r) +
			                       (Vec2r)view_region.min -
			                       (Vec2r)target_pos
			                      );
			Vec3r image_plane_position =
				image_plane_center +
				center_offset.x * image_plane_pixel_offset_x +
				center_offset.y * image_plane_pixel_offset_y;

			/////////////////////////////////////////////////////////////////////
			// Construct the primary ray
			xen::Ray3r primary_ray;
			primary_ray.origin    = image_plane_position;
			primary_ray.direction = xen::normalized(image_plane_position - params.camera.position);

			/////////////////////////////////////////////////////////////////////
			// Cast the ray into the scene
			if(!castRayIntoScene(primary_ray, scene, intersection)){
				continue;
			}

			XenDebugAssert(intersection.model_index < xen::size(scene.models),
			               "Expected intersection's object index to be within "
			               "bounds of the model list");
			XenDebugAssert(intersection.tri_index * 3 <
			               scene.models[intersection.model_index].mesh->vertex_count,
			               "Expected intersection's triangle index to be within "
			               "bounds of the vertex list");

			const RaytracerModel& model = scene.models[intersection.model_index];

			// Compute surface properties at intersection point
			xen::Color4f pixel_color        = model.color;
			Vec3r        pixel_normal_world = Vec3r::Origin;
			{
				Vec3r*      pbuf_model = model.mesh->position;
				Vec3r*      nbuf_model = model.mesh->normal;
				xen::Color* cbuf       = model.mesh->color;
				u32          buf_index = intersection.tri_index * 3;

				xen::Triangle3r ptri_model = *(xen::Triangle3r*)&pbuf_model[buf_index];

				// Get the barycentric coordinates of the intersection
				Vec3r bary = xen::getBarycentricCoordinates(ptri_model, intersection.pos_model);

				#if XEN_DEBUG_CHECKS
				// Need to check if less that something slightly negative as else
				// would get occasional false positive if any component is equal
				// to -0
				if(bary.x < -0.00001 || bary.y < -0.0001 || bary.z < -0.00001){
					XenLogFatal("Expected barycentric components to be positive, got:  "
					            "(%f, %f, %f)\n", bary.x, bary.y, bary.z);
					XenBreak();
				}
				#endif

				// If we have per vertex color information use it rather than WHITE
				if(cbuf != nullptr){
					// Extract the per vertex attributes for the triangle we have intersected

					xen::Triangle4f ctri;
					ctri.p1 = xen::makeColor4f(cbuf[buf_index + 0]);
					ctri.p2 = xen::makeColor4f(cbuf[buf_index + 1]);
					ctri.p3 = xen::makeColor4f(cbuf[buf_index + 2]);

					pixel_color *= evaluateBarycentricCoordinates(ctri, bary);
				}

				xen::Triangle3r* ntri_model = (xen::Triangle3r*)&nbuf_model[buf_index];
				xen::Triangle3r  ntri_world = *ntri_model * model.model_matrix;

				pixel_normal_world = xen::normalized(evaluateBarycentricCoordinates(ntri_world, bary) *
				                                     model.model_matrix
				                                    );
			}

			// compute light hitting surface at intersection point
			xen::Color3f total_light = params.ambient_light;
			total_light += model.emissive_color.rgb * model.emissive_color.a;
			{
				/////////////////////////////////////////////////////////////////////
				// Cast shadow ray
				for(u64 i = 0; i < params.lights.size; ++i){
					xen::Ray3r shadow_ray;

					switch(params.lights[i].type){
					case xen::LightSource3d::POINT: {
						shadow_ray.origin    = intersection.pos_world;

						shadow_ray.direction = xen::normalized(params.lights[i].point.position -
						                                       intersection.pos_world
						                                      );

						// Don't shoot directly from the surface or we may intersect
						// ourselves. Push the origin out slightly
						shadow_ray.origin += shadow_ray.direction * 0.0001_r;

						real light_dist_sq = xen::distanceSq(params.lights[i].point.position,
						                                     intersection.pos_world
						                                     );

						if(castRayIntoScene(shadow_ray, scene, shadow_intersection, true) &&
						   light_dist_sq > shadow_intersection.dist_sq_world){
							// Then there is geometry between the intersection.pos_world and
							// this light. Hence the light source is blocked
							break;
						}

						#if 0
						total_light += xsr::computeLightInfluencePhong
							( params.lights[i].point.position,
							  params.lights[i].color,
							  params.lights[i].attenuation,
							  light_dist_sq,
							  params.camera.position,
							  intersection.pos_world,
							  pixel_normal_world,
							  0, 0
							);
						#else
						total_light += xsr::computeLightInfluenceSimple
							( params.lights[i].color,
							  params.lights[i].attenuation,
							  light_dist_sq
							);
						#endif

						break;
					}
					default:
						XenLogWarn("Skipping unhandled light type in raytracer, type: %i",
						           params.lights[i].type);
						break;
					}
				}
			}

			pixel_color.rgb *= total_light;

			/////////////////////////////////////////////////////////////////////
			// Color the pixel
			target.color[target_pos.y*target.width + target_pos.x] = pixel_color;
		}
	}
} // end of renderRaytrace
} // end of namespace xsr

#endif
