////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of renderer3d rasterization
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_RASTERIZER_CPP
#define XEN_GRAPHICS_SREN_RENDERER3D_RASTERIZER_CPP

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
	// Taken from
	// http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html#algo3
	// :TODO: Write doRenderTriangle3d which takes a 3D triangle, put into clip
	// space, clip it (maybe), and then convert into screen space and call this
	void doRenderTriangle2d(xen::sren::RenderTarget& target,
		                    const xen::Aabb2r& viewport,
												const Mat4f& mvp_matrix,
	                      xen::Color color, xen::Triangle2r& tri){
		// :TODO: determine which vertex of triangle is on top, as per link.
		// Pseudo-code taken from:
		// https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
		// :NOTE: tri.p1 in the following code should be the top most vertice & tri.p3 is bottom most,
		//        assuming top left is 0,0. if bottom left is then tri.p1 should be bottom most

		//          tri.p1
		//            .
		//            |\
		//            | \
		//          a |  \
		//            |   \ b
		//            |    \
		//            |     \
		//     tri.p2 .      \
		//                    \
		//                     . tri.p3

		// :TODO: Following code plots line a, expand on this to fill triangle as in link
		Vec2r a = tri.p2 - tri.p1;
		real p_a = 2*a.y-a.x;
		real x_a = tri.p1.x;
		real y_a = tri.p1.y;

		while(x_a < a.x) {
			if (p_a>=0) {
				// :TODO: plot(x,y)
				y_a = y_a+1;
				p_a = p_a+2*a.y-2*a.x;
			} else {
				// :TODO: plot(x,y)
				p_a = p_a+2*a.y;
			}
			x_a += 1;
		}

	}
	void doRenderTriangle3d(xen::sren::RenderTarget& target,
		                    const xen::Aabb2r& viewport,
												const Mat4f& mvp_matrix,
	                      xen::Color color, xen::Triangle3r& tri){
		//:TODO:
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
					for(u32 i = 0; i < cmd->verticies.count - 1; i += 3){
						Triangle3r* tri_world = (Triangle3r*)(&cmd->verticies.verticies[i]);
						doRenderTriangle3d(target,view_region, mat_mvp, cmd->color, *tri_world);
					}
					break;
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
	}
}

#endif
