////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of renderer3d
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_CPP
#define XEN_GRAPHICS_SREN_RENDERER3D_CPP

#include <limits>
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

#include "renderer3d.hxx"

#include <cstring>
#include <cstdlib>

namespace {
	void doRenderPoints(xen::sren::RenderTarget& target,
	                    const xen::Aabb2r& viewport,
	                    const Mat4f& mvp_matrix,
	                    xen::Color color,
	                    Vec3r* points, u32 count){

		for(u32 i = 0; i < count; ++i){
			Vec4f clip_space = xen::mkVec(points[i], 1_r) * mvp_matrix;

			if(clip_space.z < 0){
				// Then point is behind the camera
				continue;
			}

			///////////////////////////////////////////////////////////////////
			// Do perspective divide (into normalized device coordinates -> [-1, 1]
			// We only care about x and y coordinates at this point
			clip_space.xy /= (clip_space.w);
			///////////////////////////////////////////////////////////////////

			///////////////////////////////////////////////////////////////////
			// Transform into screen space
			Vec2r screen_space = clip_space.xy;

			screen_space += Vec2r{1,1};                  // convert to [0, 2] space
			screen_space /= 2.0_r;                       // convert to [0. 1] space
			screen_space *= viewport.max - viewport.min; // convert to screen space
			screen_space += viewport.min;
			///////////////////////////////////////////////////////////////////

			if(screen_space.x < viewport.min.x ||
			   screen_space.y < viewport.min.y ||
			   screen_space.x > viewport.max.x ||
			   screen_space.y > viewport.max.y){
				continue;
			}

			target[screen_space.x][screen_space.y] = color;
		}
	}

	void doRenderLine2d(xen::sren::RenderTarget& target, xen::LineSegment2r line, xen::Color color){
		//https://www.cs.virginia.edu/luther/blog/posts/492.html
		if(line.p1 != line.p2){
			//printf("%f, %f  ->  %f, %f\n", line.p1.x, line.p1.y, line.p2.x, line.p2.y);
			real num_pixels = xen::max(abs(line.p1.x - line.p2.x), abs(line.p1.y - line.p2.y));
			//printf("Drawing line with %f (%u) pixels\n", num_pixels, (u32)num_pixels);
			Vec2r delta = (line.p1 - line.p2) / num_pixels;
			Vec2r cur   = line.p2;
			for(u32 i = 0; i < (u32)num_pixels; ++i){
				target[cur.x][cur.y] = color;
				cur += delta;
			}
		}
	}

	void doRenderLine3d(xen::sren::RenderTarget& target,
	                    const xen::Aabb2r& viewport,
	                    const Mat4f& mvp_matrix,
	                    xen::Color color,
	                    const xen::LineSegment3r& line){

		xen::LineSegment4r line_clip  = xen::getTransformed(xen::toHomo(line), mvp_matrix);

		///////////////////////////////////////////////////////////////////
		// Do line clipping
		if(line_clip.p1.z < 0 && line_clip.p2.z < 0){
			// Then line is completely behind the camera
			return;
		}
		if(line_clip.p1.z < 0){
			// Then just one point is behind the camera, slide it along the direction
			// of the line until its at same z as camera
			Vec4r dir = line_clip.p2 - line_clip.p1;
			line_clip.p1 += dir * ((-line_clip.p1.z) / dir.z);
		}
		if(line_clip.p2.z < 0){
			Vec4r dir = line_clip.p1 - line_clip.p2;
			line_clip.p2 += dir * ((-line_clip.p2.z) / dir.z);
		}
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Do perspective divide (into normalized device coordinates -> [-1, 1]
		// We only care about x and y coordinates at this point
		line_clip.p1.xy /= (line_clip.p1.w);
		line_clip.p2.xy /= (line_clip.p2.w);
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Transform into screen space
		xen::LineSegment2r  line_screen;
		line_screen.p1 = line_clip.p1.xy;
		line_screen.p2 = line_clip.p2.xy;

		// :TODO:COMP:ISSUE_5: define operators on line_segment, triangle etc (treat them as
		// multiple rows of row vectors and do matrix multiply, IE, line segment is:
		// [ x1 y1 z1 w1 ]   [ a b c d ]   [ x1' y1' z1' w1' ]
		// [ x2 y2 z2 w2 ] * [ e f g h ] = [ x2' y2' z2' w2' ]
		//                   [ i j k l ]
		//                   [ m n o p ]
		line_screen.p1 += Vec2r{1,1};                  // convert to [0, 2] space
		line_screen.p1 /= 2.0_r;                       // convert to [0. 1] space
		line_screen.p1 *= viewport.max - viewport.min; // convert to screen space
		line_screen.p1 += viewport.min;
		line_screen.p2 += Vec2r{1,1};                  // convert to [0, 2] space
		line_screen.p2 /= 2.0_r;                       // convert to [0. 1] space
		line_screen.p2 *= viewport.max - viewport.min; // convert to screen space
		line_screen.p2 += viewport.min;
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Clip to the viewport
		if(xen::intersect(line_screen, viewport)){
			doRenderLine2d(target, line_screen, color);
		}
		///////////////////////////////////////////////////////////////////
	}
}

namespace xen{

	namespace sren {
		void clear(RenderTarget& target, Color color){
			static_assert(sizeof(Color) == sizeof(int), "Relying on memset which takes int values");

			memset(target.pixels, color.value, target.width * target.height * sizeof(Color));
		}

		void clear(RenderTarget& target, const xen::Aabb2u& viewport, Color color) {
			for(u32 x = viewport.min.x; x < viewport.max.x; ++x){
				for(u32 y = viewport.min.y; y < viewport.max.y; ++y){
					target[x][y] = color;
				}
			}
		}

		void renderRasterize(RenderTarget& target, const xen::Aabb2u& viewport,
		                     const Camera3d& camera,
		                     RenderCommand3d* commands, u32 command_count){

			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
			xen::Aabb2u screen_rect = { Vec2u::Origin, target.size - Vec2u{1,1} };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

			Mat4r mat_vp = xen::getViewProjectionMatrix(camera, view_region.max - view_region.min);
			Mat4r mat_mvp;

			int stride = 0;

			RenderCommand3d* cmd = commands;
			for(u32 cmd_index = 0; cmd_index < command_count; ++cmd_index){
				cmd = &commands[cmd_index];
				mat_mvp = cmd->model_matrix * mat_vp;
				switch(cmd->type){
				case RenderCommand3d::POINTS:
					doRenderPoints(target, view_region, mat_mvp, cmd->color, cmd->verticies.verticies, cmd->verticies.count);
					break;
				case RenderCommand3d::LINES:
					stride = 2;
					goto do_render_lines;
					break;
				case RenderCommand3d::TRIANGLES: // :TODO: rasterize this
				case RenderCommand3d::LINE_STRIP:
					stride = 1;
				do_render_lines:
					for(u32 i = 0; i < cmd->verticies.count - 1; i += stride){
						//printf("Doing vertex %i / %i\n", i, cmd->verticies.count);
						LineSegment3r* line_world = (LineSegment3r*)(&cmd->verticies.verticies[i]);

						doRenderLine3d(target, view_region, mat_mvp, cmd->color, *line_world);
					}
					break;
				default:
					XenInvalidCodePath();
					break;
				}
			}
		}

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

			printf("offset_x: (%8f, %8f, %8f), offset_y: (%8f, %8f, %8f)\n",
			       image_plane_pixel_offset_x.x, image_plane_pixel_offset_x.y, image_plane_pixel_offset_x.z,
			       image_plane_pixel_offset_y.x, image_plane_pixel_offset_y.y, image_plane_pixel_offset_y.z
			      );

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

						Vec3r closest_intersection = Vec3r::Origin;
						real closest_intersection_length = std::numeric_limits<real>::max();
						bool found_intersection    = false;

						for(u32 i = 0; i < cmd->verticies.count; i += 3){
							tri = (const Triangle3r*)&cmd->verticies.verticies[i];

							Vec3r intersection;
							real intersection_length;

							if(xen::getIntersection(primary_ray_model_space, *tri, intersection)){
								// :TODO: visually inspect this with cornell box
								intersection_length = distanceSq(camera.position, intersection);
								if(closest_intersection_length > intersection_length){
									closest_intersection = intersection;
								}
								found_intersection = true;
							}
						}

						if(found_intersection){
							// :TODO: target_size.y - target_pos.y is a hack because everything is reflected in y currently
							Vec2u pixel_coord { target_pos. x, target_size.y - target_pos.y };
							pixel_coord += (Vec2u)view_region.min;
							target[pixel_coord.x][pixel_coord.y] = cmd->color; //Color::WHITE;
						}
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


			Vec3r axis_line[] = {
				Vec3r::Origin, Vec3r::UnitX,
			};

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
