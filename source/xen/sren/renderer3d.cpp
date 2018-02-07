////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \file renderer3d.cpp
/// \author Jamie Terry
/// \date 2018/01/25
/// \brief Contains implementation of renderer3d
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_CPP
#define XEN_GRAPHICS_SREN_RENDERER3D_CPP

#include <xen/math/geometry.hpp>
#include <xen/sren/renderer3d.hxx>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

#include <cstring>
#include <cstdlib>

namespace {
	void doRenderPoints(xen::sren::RenderTarget& target, const Mat4f& mvp_matrix,
	                    xen::Color color,
	                    Vec3r* points, u32 count){

		for(u32 i = 0; i < count; ++i){
			Vec4f clip_space = xen::mkVec(points[i], 1_r) * mvp_matrix;

			if(clip_space.z < 0){
				// Then point is behind the camera
				continue;
			}

			Vec2f screen_space = (clip_space.xy / clip_space.w) + ((Vec2r)target.size * 0.5_r);

			if(screen_space.x < 0 ||
			   screen_space.y < 0 ||
			   screen_space.x > target.size.x ||
			   screen_space.y > target.size.y){
				continue;
			}

			target[screen_space.x][screen_space.y] = color;
		}
	}

	void doRenderLine(xen::sren::RenderTarget& target, xen::LineSegment2r line, xen::Color color){
		//https://www.cs.virginia.edu/luther/blog/posts/492.html
		if(line.p1 != line.p2){
			//printf("%f, %f  ->  %f, %f\n", line.p1.x, line.p1.y, line.p2.x, line.p2.y);
			real num_pixels = XenMax(abs(line.p1.x - line.p2.x), abs(line.p1.y - line.p2.y));
			//printf("Drawing line with %f (%u) pixels\n", num_pixels, (u32)num_pixels);
			Vec2r delta = (line.p1 - line.p2) / num_pixels;
			Vec2r cur   = line.p2;
			for(u32 i = 0; i < (u32)num_pixels; ++i){
				target[cur.x][cur.y] = color;
				cur += delta;
			}
		}
	}
}

namespace xen{

	namespace sren {
		void clear(RenderTarget& target, Color color){
			static_assert(sizeof(Color) == sizeof(int), "Relying on memset which takes int values");

			memset(target.pixels, color.value, target.width * target.height * sizeof(Color));
		}

		void renderRasterize(RenderTarget& target, const Camera3d& camera, RenderCommand3d* commands, u32 command_count){
			Mat4r mat_vp = xen::getViewProjectionMatrix(camera, xen::mkVec(1_r, 1_r));
			Mat4r mat_mvp;

		  xen::Aabb2r screen_rect = { Vec2r::Origin, (Vec2r)target.size - xen::mkVec(1_r, 1_r) };

			int stride = 0;

			RenderCommand3d* cmd = commands;
			for(u32 cmd_index = 0; cmd_index < command_count; ++cmd_index){
				cmd = &commands[cmd_index];
				printf("Executing render command: %u\n", cmd_index);
				mat_mvp = cmd->model_matrix * mat_vp;
				switch(cmd->type){
				case RenderCommand3d::POINTS:
					doRenderPoints(target, mat_mvp, cmd->color, cmd->verticies.verticies, cmd->verticies.count);
					break;
				case RenderCommand3d::LINES:
					stride = 2;
					goto do_render_lines;
					break;
				case RenderCommand3d::LINE_STRIP:
					stride = 1;
				do_render_lines:
					for(u32 i = 0; i < cmd->verticies.count - 1; i += stride){
						//printf("Doing vertex %i / %i\n", i, cmd->verticies.count);
						LineSegment3r* line_world = (LineSegment3r*)(&cmd->verticies.verticies[i]);
						//printf("%f, %f, %f  --- %f, %f, %f\n",
						//       line_world->p1.x, line_world->p1.y, line_world->p1.z,
							        //       line_world->p2.x, line_world->p2.y, line_world->p2.z);

						LineSegment3r  line_clip  = xen::getTransformed(*line_world, mat_mvp);

						//printf("%f, %f, %f  --- %f, %f, %f\n",
						//       line_clip.p1.x, line_clip.p1.y, line_clip.p1.z,
							        //       line_clip.p2.x, line_clip.p2.y, line_clip.p2.z);

						LineSegment2r  line_screen;
						line_screen.p1 = line_clip.p1.xy + ((Vec2r)target.size * 0.5_r);
						line_screen.p2 = line_clip.p2.xy + ((Vec2r)target.size * 0.5_r);

						//printf("%f, %f  --- %f, %f\n", line_clip.p1.x, line_clip.p1.y, line_clip.p2.x, line_clip.p2.y);
						if(xen::intersect(line_screen, screen_rect)){
							doRenderLine(target, line_screen, cmd->color);
						}
					}
					break;
				case RenderCommand3d::TRIANGLES:
					break;
				default:
					XenInvalidCodePath();
					break;
				}
			}
		}

		void renderRaytrace (RenderTarget& target, const Camera3d& camera, RenderCommand3d* commands, u32 command_count){
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
				xen::tan(fov_y / (real)target_size.y) * camera.z_near, 0, 0
			};

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



				}
			}

		}
	}


}

#endif
