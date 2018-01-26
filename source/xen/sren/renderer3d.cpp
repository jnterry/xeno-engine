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
			real num_pixels = XenMax(abs(line.p1.x - line.p2.x), abs(line.p1.y - line.p2.y));
			Vec2r delta = (line.p1 - line.p2) / num_pixels;
			Vec2r cur   = line.p2;
			for(u32 i = 0; i < (u32)num_pixels; ++i){
				// :TODO: we should be cliping the line to screen space at call point...
				if(cur.x < 0 || cur.y < 0 || cur.x >= target.size.x || cur.y >= target.size.y){
					continue;
				}
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

			xen::Aabb2r screen_rect = { Vec2r::Origin, (Vec2r)target.size };

			RenderCommand3d* cmd = commands;
			for(int i = 0; i < command_count; cmd = &commands[i], ++i){
				switch(cmd->type){
				case RenderCommand3d::POINTS:
					doRenderPoints(target, camera, cmd->color, cmd->verticies.verticies, cmd->verticies.count);
					break;
				case RenderCommand3d::LINE_STRIP:
					for(int i = 0; i < cmd->verticies.count - 1; ++i){
						LineSegment3r* line_world = (LineSegment3r*)(&cmd->verticies.verticies[i]);
						LineSegment3r  line_clip  = xen::getTransformed(*line_world, mat_vp);

						LineSegment2r  line_screen;
						line_screen.p1 = line_clip.p1.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * (Vec2r)target.size;
						line_screen.p2 = line_clip.p2.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * (Vec2r)target.size;

						xen::intersect(line_screen, screen_rect);

						doRenderLine(target, line_screen, cmd->color);
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
