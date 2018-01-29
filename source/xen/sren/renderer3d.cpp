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
	void doRenderPoints(xen::sren::RenderTarget& target, const xen::Camera3d& camera,
	                    xen::Color color,
	                    Vec3r* points, u32 count){

		Mat4f mat_vp = xen::getViewProjectionMatrix(camera, (Vec2r)target.size);

		for(u32 i = 0; i < count; ++i){
			Vec3f clip_space = points[i] * mat_vp;

			/*if(clip_space.x < -1 || clip_space.x > 1 ||
			  clip_space.y < -1 || clip_space.y > 1 ||
			  clip_space.z < -1 || clip_space.z > 1){
			  printf("%f, %f, %f\n", clip_space.x, clip_space.y, clip_space.z);
			  continue;
			  }*/

			// :TODO: is this needed?
			//printf("%f, %f, %f\n", clip_space.x, clip_space.y, clip_space.z);
			//clip_space /= clip_space.z;

			Vec2f screen_space = clip_space.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * (Vec2r)target.size;

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

			Mat4f mat_vp = xen::getViewProjectionMatrix(camera, (Vec2r)target.size);

			xen::Aabb2r screen_rect = { Vec2r::Origin, (Vec2r)target.size - ((Vec2r){1,1}) };

			int stride = 0;

			RenderCommand3d* cmd = commands;
			for(u32 cmd_index = 0; cmd_index < command_count; ++cmd_index){
				cmd = &commands[cmd_index];
				printf("Executing render command: %u\n", cmd_index);
				switch(cmd->type){
				case RenderCommand3d::POINTS:
					doRenderPoints(target, camera, cmd->color, cmd->verticies.verticies, cmd->verticies.count);
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

						LineSegment3r  line_clip  = xen::getTransformed(*line_world, mat_vp);

						//printf("%f, %f, %f  --- %f, %f, %f\n",
						//       line_clip.p1.x, line_clip.p1.y, line_clip.p1.z,
							        //       line_clip.p2.x, line_clip.p2.y, line_clip.p2.z);

						LineSegment2r  line_screen;
						line_screen.p1 = line_clip.p1.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * (Vec2r)target.size;
						line_screen.p2 = line_clip.p2.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * (Vec2r)target.size;

						//printf("%f, %f  --- %f, %f\n", line_clip.p1.x, line_clip.p1.y, line_clip.p2.x, line_clip.p2.y);
						if(xen::intersect(line_screen, screen_rect)){
							doRenderLine(target, line_screen, cmd->color);
						}
					}
					break;
				default:
					XenInvalidCodePath();
					break;
				}
			}
		}

		void renderRaytrace (RenderTarget& target, const Camera3d& camera, RenderCommand3d* commands, u32 command_count){
			renderRasterize(target, camera, commands, command_count);
		}
	}


}

#endif
