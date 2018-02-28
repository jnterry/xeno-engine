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
#include <xen/math/vertex_group.hpp>
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
	                    xen::Color4f color,
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
			screen_space += viewport.min;                // move to correct part of screen
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

	void doRenderLine2d(xen::sren::RenderTarget& target, xen::LineSegment2r line, xen::Color4f color){
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
			/*
			// Bresenham Algorithm, taken from:
			// https://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#C.2B.2B
			// :NOTE: May be a more efficient implementation available
			const bool steep = (fabs(line.p2.y - line.p1.y) > fabs(line.p2.x - line.p1.x));
			if(steep){
    		// Swap (line.p1.x, line.p1.y)
				line.p1.x = line.p1.x + line.p1.y;
				line.p1.y = line.p1.x - line.p1.y;
				line.p1.x = line.p1.x - line.p1.y;
				// Swap (line.p2.x, line.p2.y)
				line.p2.x = line.p2.x + line.p2.y;
				line.p2.y = line.p2.x - line.p2.y;
				line.p2.x = line.p2.x - line.p2.y;
  		}
			if(line.p1.x > line.p2.x){
				// Swap (line.p1.x, line.p2.x)
				line.p1.x = line.p1.x + line.p2.x;
				line.p2.x = line.p1.x - line.p2.x;
				line.p1.x = line.p1.x - line.p2.x;
				// Swap (line.p1.y, line.p2.y)
				line.p1.y = line.p1.y + line.p2.y;
				line.p2.y = line.p1.y - line.p2.y;
				line.p1.y = line.p1.y - line.p2.y;
 			}
			const real dx = line.p2.x - line.p1.x;
  		const real dy = fabs(line.p2.y - line.p1.y);

  		real error = dx / 2.0f;
  		const int ystep = (line.p1.y < line.p2.y) ? 1 : -1;
  		int y = (int)line.p1.y;
			const int maxX = (int)line.p2.x;

			for(int x=(int)line.p1.x; x<maxX; x++){
    		if(steep){
						target[y][x] = color;
    		}else{
        		target[x][y] = color;
    		}

    		error -= dy;
    		if(error < 0){
        	y += ystep;
        	error += dx;
    		}
  		}
			*/
		}
	}

	void doRenderLine3d(xen::sren::RenderTarget& target,
	                    const xen::Aabb2r& viewport,
	                    const Mat4f& mvp_matrix,
	                    xen::Color4f color,
	                    const xen::LineSegment3r& line){

		xen::LineSegment4r line_clip  = xen::toHomo(line) * mvp_matrix;

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

		// :TODO:COMP:ISSUE_15: swizzle function
		// Once we can do generic swizzles, we could make transform to screen space
		// a template function and use same code for this and points and triangles, etc
		line_screen.p1 = line_clip.p1.xy;
		line_screen.p2 = line_clip.p2.xy;

		line_screen += Vec2r{1,1};                  // convert to [0, 2] space
		line_screen /= 2.0_r;                       // convert to [0, 1] space
		line_screen *= viewport.max - viewport.min; // convert to screen space
		line_screen += viewport.min;                // move to correct part of screen
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Clip to the viewport
		if(xen::intersect(line_screen, viewport)){
			doRenderLine2d(target, line_screen, color);
		}
		///////////////////////////////////////////////////////////////////
	}
	// Bresenham Triangle Algorithm, inspired by
	// http://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html#algo3
	// :NOTE: tri.p1 becomes the bottom most vertice & tri.p3 is top most, as (0,0) is bottom left
	//                     . tri.p3
	//                    /
	//     tri.p2 .      /
	//            |     /
	//            |    /
	//            |   / a
	//          b |  /
	//            | /
	//            |/
	//            .
	//          tri.p1
	void doRenderTriangle2d(xen::sren::RenderTarget& target,
		                      const xen::Aabb2r& viewport,
	                        xen::Color4f color, xen::Triangle2r& tri){
		// If any two points are equal draw a line and bail out
		if (tri.p1 == tri.p2){
			xen::LineSegment2r line = {tri.p1, tri.p3};
			// Clip to the viewport
			if(xen::intersect(line, viewport)){
				doRenderLine2d(target, line, color);
			}
			return;
		} else if (tri.p1 == tri.p3){
			xen::LineSegment2r line = {tri.p1, tri.p2};
			// Clip to the viewport
			if(xen::intersect(line, viewport)){
				doRenderLine2d(target, line, color);
			}
			return;
		} else if (tri.p2 == tri.p3){
			xen::LineSegment2r line = {tri.p1, tri.p3};
			// Clip to the viewport
			if(xen::intersect(line, viewport)){
				doRenderLine2d(target, line, color);
			}
			return;
		}
		// Sort Points
		Vec2r temp;
		if (tri.p1.y > tri.p2.y){
			temp = tri.p1;
			tri.p1 = tri.p2;
			tri.p2 = temp;
		}
		if (tri.p2.y > tri.p3.y){
			temp = tri.p2;
			tri.p2 = tri.p3;
			tri.p3 = temp;
		}
		if (tri.p1.y > tri.p2.y){
			temp = tri.p1;
			tri.p1 = tri.p2;
			tri.p2 = temp;
		}
		//printf("Points of triangle: (%f,%f), (%f,%f), (%f,%f)\n", tri.p1.x, tri.p1.y,tri.p2.x, tri.p2.y,tri.p3.x, tri.p3.y);
		// Create line for each of a, b, & c for drawing purposes
		xen::LineSegment2r line_a = {tri.p1, tri.p3};
		xen::LineSegment2r line_b = {tri.p1, tri.p2};
		xen::LineSegment2r line_c = {tri.p2, tri.p3};
		// Find step vector for line_a as well as start position
		real num_pixels_a = xen::max(abs(line_a.p1.x - line_a.p2.x), abs(line_a.p1.y - line_a.p2.y));
		printf("Number of pixels in line_a: %f\n", num_pixels_a);
		Vec2r delta_a = (line_a.p2 - line_a.p1) / num_pixels_a;
		Vec2r curr_a  = line_a.p1;
		Vec2r prev_a;
		bool stepped_y_a = false;
		// Find step vector for line_b as well as start position
		real num_pixels_b = xen::max(abs(line_b.p1.x - line_b.p2.x), abs(line_b.p1.y - line_b.p2.y));
		printf("Number of pixels in line_b: %f\n", num_pixels_b);
		Vec2r delta_b = (line_b.p2 - line_b.p1) / num_pixels_b;
		Vec2r curr_b  = line_b.p1;
		Vec2r prev_b;
		bool stepped_y_b = false;
		// Find step vector for line_b, no start position as c is drawn after b is finished
		// (line_c.start == line_b.end)
		real num_pixels_c = xen::max(abs(line_c.p1.x - line_c.p2.x), abs(line_c.p1.y - line_c.p2.y));
		printf("Number of pixels in line_c: %f\n", num_pixels_c);
		Vec2r delta_c = (line_c.p2 - line_c.p1) / num_pixels_c;
		// :TODO: This may need to be just < or != etc, idek
		for(u32 i = 0; i < (u32)num_pixels_a;){
		//while ((curr_b.x <= line_a.p2.x) && (curr_b.y <= line_a.p2.y)){
			if (!stepped_y_a){
				if (xen::contains(viewport, curr_a)){
					target[curr_a.x][curr_a.y] = color;
				}
				prev_a = curr_a;
				curr_a += delta_a;
				// Equiv: stepped_y_a = ((u32)curr_a.y != (u32)prev_a.y) ? true : false;
				stepped_y_a = !((u32)curr_a.y - (u32)prev_a.y);
				++i;
			}
			while (!stepped_y_b){
				if (xen::contains(viewport, curr_b)){
					target[curr_b.x][curr_b.y] = color;
				}
				prev_b = curr_b;
				curr_b += delta_b;
				// Equiv: stepped_y_b = ((u32)curr_b.y != (u32)prev_b.y) ? true : false;
				stepped_y_b = !((u32)curr_b.y - (u32)prev_b.y);
			}
			// :TODO: This if may need to go inside, or above, the above while
			// If b has reached end point then begin drawing line c by changing vector
			// that represents a single step from that of b to that of c
			if (curr_b == line_b.p2){
				delta_b = delta_c;
			}
			if (stepped_y_a && stepped_y_b){
				//printf("Stuck in the connecting loop\n");
				//printf("Val of curr_a: %f, Val of curr_b: %f\n", curr_a.x, curr_b.x);
				// draw horizontal line
				if (curr_a.x < curr_b.x){
					for (int x=curr_a.x; x<curr_b.x; ++x){
						//printf("Val of x: %i\n", x);
						if (xen::contains(viewport, xen::mkVec((real)x,curr_a.y))){
							target[x][curr_a.y] = color;
						}
					}
				}else{
					for (int x=curr_b.x; x<curr_a.x; ++x){
						//printf("Val of x: %i\n", x);
						if (xen::contains(viewport, xen::mkVec((real)x,curr_a.y))){
							target[x][curr_a.y] = color;
						}
					}
				}
				stepped_y_a = false;
				stepped_y_b = false;
			}
		}

	}
	// :TODO: Write doRenderTriangle3d which takes a 3D triangle, put into clip
	// space, clip it (maybe), and then convert into screen space and call doRenderTriangle2d
	void doRenderTriangle3d(xen::sren::RenderTarget& target,
		                      const xen::Aabb2r& viewport,
												  const Mat4f& mvp_matrix,
	                        xen::Color4f color, xen::Triangle3r& tri){
		xen::Triangle4r tri_clip  = xen::toHomo(tri) * mvp_matrix;
		///////////////////////////////////////////////////////////////////
		// Do perspective divide (into normalized device coordinates -> [-1, 1]
		// We only care about x and y coordinates at this point
		tri_clip.p1.xy /= (tri_clip.p1.w);
		tri_clip.p2.xy /= (tri_clip.p2.w);
		tri_clip.p3.xy /= (tri_clip.p3.w);
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Transform into screen space
		xen::Triangle2r  tri_screen;

		// :TODO:COMP:ISSUE_15: swizzle function
		// Once we can do generic swizzles, we could make transform to screen space
		// a template function and use same code for this and points and triangles, etc
		tri_screen.p1 = tri_clip.p1.xy;
		tri_screen.p2 = tri_clip.p2.xy;
		tri_screen.p3 = tri_clip.p3.xy;

		tri_screen += Vec2r{1,1};                  // convert to [0, 2] space
		tri_screen /= 2.0_r;                       // convert to [0, 1] space
		tri_screen *= viewport.max - viewport.min; // convert to screen space
		tri_screen += viewport.min;                // move to correct part of screen
		///////////////////////////////////////////////////////////////////
		//printf("Points of triangle in model space: (%f,%f,%f), (%f,%f,%f), (%f,%f,%f)\n", tri.p1.x, tri.p1.y, tri.p1.z,tri.p2.x, tri.p2.y, tri.p2.z,tri.p3.x, tri.p3.y, tri.p3.z);
		//printf("Points of triangle in clip space: (%f,%f,%f), (%f,%f,%f), (%f,%f,%f)\n", tri_clip.p1.x, tri_clip.p1.y, tri_clip.p1.z,tri_clip.p2.x, tri_clip.p2.y, tri_clip.p2.z,tri_clip.p3.x, tri_clip.p3.y, tri_clip.p3.z);
		//printf("Points of triangle in screen space: (%f,%f), (%f,%f), (%f,%f)\n", tri_screen.p1.x, tri_screen.p1.y,tri_screen.p2.x, tri_screen.p2.y,tri_screen.p3.x, tri_screen.p3.y);
		doRenderTriangle2d(target, viewport, color, tri_screen);
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
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands){

			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
			xen::Aabb2u screen_rect = { Vec2u::Origin, target.size - Vec2u{1,1} };
			xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

			Mat4r mat_vp = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);
			Mat4r mat_mvp;

			int stride = 0;

			const RenderCommand3d* cmd;
			for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
				cmd = &commands[cmd_index];

				mat_mvp = cmd->model_matrix * mat_vp;

				Color4f base_color = cmd->color;
				base_color.rgb *= params.ambient_light;

				switch(cmd->type){
				case RenderCommand3d::POINTS:
					doRenderPoints(target, view_region, mat_mvp, base_color, cmd->verticies.verticies, cmd->verticies.count);
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

						doRenderLine3d(target, view_region, mat_mvp, base_color, *line_world);
					}
					break;
				default:
					XenInvalidCodePath("Unhandled render command type in software rasterizer");
					break;
				}
			}
		}
	}
}

#endif
