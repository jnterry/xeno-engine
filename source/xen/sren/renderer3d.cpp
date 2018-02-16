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

		// :TODO: define operators on line_segment, triangle etc (treat them as
		// multiple rows of row vectors and do matrix multiply, IE, line segment is:
		// [ x1 y1 z1 w1 ]   [ a b c d ]   [ x1' y1' z1' w1' ]
		// [ x2 y2 z2 w2 ] * [ e f g h ] = [ x2' y2' z2' w2' ]
		//                   [ i j k l ]
		//                   [ m n o p ]
		line_screen.p1 += Vec2r{1,1};                  // convert to [0, 2] space
		line_screen.p1 /= 2.0_r;                       // convert to [0. 1] space
		line_screen.p1 *= viewport.max - viewport.min; // convert to screen space
		line_screen.p2 += Vec2r{1,1};                  // convert to [0, 2] space
		line_screen.p2 /= 2.0_r;                       // convert to [0. 1] space
		line_screen.p2 *= viewport.max - viewport.min; // convert to screen space
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
				printf("Executing render command: %u\n", cmd_index);
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

			// :TODO: use viewport

			Vec2s target_size = (Vec2s)target.size;

			Angle fov_y = camera.fov_y;
			Angle fov_x = camera.fov_y * ((real)target_size.y / (real)target_size.x);

			// Compute the image plane center, and offset between each pixel
			// Start with doing this in camera space (so easy conceptually),
			// then transform into world space by lining up z axis with
			// camera's look_dir
			Vec3r image_plane_center       = { 0, 0, camera.z_near };

			// This is the offset between pixels on the image plane
			//
			//            x
			//         _______
			//         |     /
			//         |    /
			//  z_near |   /
			//         |  /
			//         | /
			//         |/ angle = fov_x / target_width
			Vec3r image_plane_pixel_offset_x = {
				xen::tan(fov_x / (real)target_size.x) * camera.z_near, 0, 0
			};
			Vec3r image_plane_pixel_offset_y = {
				0, xen::tan(fov_y / (real)target_size.y) * camera.z_near, 0
			};

			xen::Quaternion camera_rotation = xen::getRotation(xen::normalized(image_plane_center),
			                                                   xen::normalized(camera.look_dir)
			                                                  );

			image_plane_pixel_offset_x = xen::rotated(image_plane_pixel_offset_x, camera_rotation);
			image_plane_pixel_offset_y = xen::rotated(image_plane_pixel_offset_y, camera_rotation);

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
							target[target_pos.x][target_pos.y] = Color::WHITE;
						}
					}
				}
			}
		}
	}


}

#endif
