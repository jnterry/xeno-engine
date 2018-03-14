////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of renderer3d rasterization
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RENDERER3D_RASTERIZER_CPP
#define XEN_GRAPHICS_SREN_RENDERER3D_RASTERIZER_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/vertex_group.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

#include "renderer3d.hxx"
#include "RenderTargetImpl.hxx"

#include <cstring>
#include <cstdlib>
#include <float.h>

namespace {
	template<typename T_IN, typename T_OUT>
  T_OUT _convertToScreenSpace(const T_IN& in, const xen::Aabb2r& viewport){
		T_OUT out = xen::swizzle<'x','y'>(in);

		out += Vec2r{1,1};                  // convert to [0, 2] space
		out /= 2.0_r;                       // convert to [0, 1] space
		out *= viewport.max - viewport.min; // convert to screen space
		out += viewport.min;                // move to correct part of screen

		return out;
	}

	void doRenderPoints(xen::sren::RenderTargetImpl& target,
	                    const xen::Aabb2r& viewport,
	                    const Mat4f& mvp_matrix,
	                    const xen::ImmediateGeometrySource& geom){

		const xen::Color* color_buffer = geom.color;
		if(geom.color == nullptr){
			color_buffer = &xen::Color::WHITE;
		}

		for(u32 i = 0; i < geom.vertex_count; ++i){
			Vec4f clip_space = xen::mkVec(geom.position[i], 1_r) * mvp_matrix;

			if(clip_space.z < 0){
				// Then point is behind the camera
				continue;
			}
			real depth = clip_space.z;

			///////////////////////////////////////////////////////////////////
			// Do perspective divide (into normalized device coordinates -> [-1, 1]
			// We only care about x and y coordinates at this point
			clip_space.xy /= (clip_space.w);
			///////////////////////////////////////////////////////////////////

			Vec2r screen_space =
				_convertToScreenSpace<Vec4r, Vec2r>(clip_space, viewport);
			if(screen_space.x < viewport.min.x ||
			   screen_space.y < viewport.min.y ||
			   screen_space.x > viewport.max.x ||
			   screen_space.y > viewport.max.y){
				continue;
			}

			u32 pixel_index = (u32)screen_space.y*target.width + (u32)screen_space.x;

			if (depth > target.depth[pixel_index]){
				// Then point is behind something else occupying this pixel
				continue;
			}
			target.depth[pixel_index] = depth;
			target.color[pixel_index] = xen::makeColor4f(color_buffer[i * (geom.color != nullptr)]);
		}
	}

	void doRenderLine2d(xen::sren::RenderTargetImpl& target, xen::LineSegment2r line,
	                    xen::Color4f color1,
											xen::Color4f color2,
	                    real z_start, real z_end){
		if(line.p1 == line.p2){ return; }

		#if 1
		//https://www.cs.virginia.edu/luther/blog/posts/492.html

		//printf("%f, %f  ->  %f, %f\n", line.p1.x, line.p1.y, line.p2.x, line.p2.y);
		real num_pixels = xen::max(abs(line.p1.x - line.p2.x), abs(line.p1.y - line.p2.y));
		//printf("Drawing line with %f (%u) pixels\n", num_pixels, (u32)num_pixels);
		Vec2r delta = (line.p1 - line.p2) / num_pixels;
		Vec2r cur   = line.p2;
		for(u32 i = 0; i < (u32)num_pixels; ++i){

			// :TODO: Replace lerp with a perspective correct interpolation
			real depth = xen::lerp(z_start, z_end, (i/num_pixels));
			xen::Color4f color = xen::lerp(color1, color2, (i/num_pixels));
			target.color[(u32)cur.y*target.width + (u32)cur.x] = color;
			target.depth[(u32)cur.y*target.width + (u32)cur.x] = depth;
			cur += delta;
		}

		#else

		// Bresenham Algorithm, taken from:
		// https://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#C.2B.2B
		// :NOTE: May be a more efficient implementation available
		const bool steep = (fabs(line.p2.y - line.p1.y) > fabs(line.p2.x - line.p1.x));
		if(steep){
			xen::swap(line.p1.x, line.p1.y);
			xen::swap(line.p2.x, line.p2.y);
		}
		if(line.p1.x > line.p2.x){
			xen::swap(line.p1.x, line.p2.x);
			xen::swap(line.p1.y, line.p2.y);
		}
		const real dx = line.p2.x - line.p1.x;
		const real dy = fabs(line.p2.y - line.p1.y);

		real error = dx / 2.0f;
		const int ystep = (line.p1.y < line.p2.y) ? 1 : -1;
		int y = (int)line.p1.y;
		const int maxX = (int)line.p2.x;

		for(int x=(int)line.p1.x; x<maxX; x++){
			if(steep){
				target.color[(u32)x*target.width + y] = color;
			}else{
				target.color[(u32)y*target.width + x] = color;
			}

			error -= dy;
			if(error < 0){
				y += ystep;
				error += dx;
			}
		}
		#endif
	}

	void doRenderLine3d(xen::sren::RenderTargetImpl& target,
	                    const xen::Aabb2r& viewport,
	                    const Mat4f& mvp_matrix,
	                    xen::Color4f color1,
											xen::Color4f color2,
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
		real z_start = line_clip.p2.z;
		real z_end   = line_clip.p1.z;
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Do perspective divide (into normalized device coordinates -> [-1, 1]
		// We only care about x and y coordinates at this point
		line_clip.p1.xy /= (line_clip.p1.w);
		line_clip.p2.xy /= (line_clip.p2.w);
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Clip to the viewport
		xen::LineSegment2r line_screen =
			_convertToScreenSpace<xen::LineSegment4r, xen::LineSegment2r>(line_clip, viewport);
		if(xen::intersect(line_screen, viewport)){
			doRenderLine2d(target, line_screen, color1, color2, z_start, z_end);
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
	void doRenderTriangle2d(xen::sren::RenderTargetImpl& target,
		                      const xen::Aabb2r& viewport,
	                        xen::Color4f color, xen::Triangle2r tri){

		{ // If any two points are equal draw a line and bail out
			xen::LineSegment2r* line = nullptr;
			if(tri.p1 == tri.p2){
				// then draw line connecting p2 and p3
				line = (xen::LineSegment2r*)&tri.vertices[1];
			} else if (tri.p1 == tri.p3 || tri.p2 == tri.p3) {
				// then draw line connecting p1 and p2
				line = (xen::LineSegment2r*)&tri.vertices[0];
			}
			if(line) {
				// Then the triangle has at least 2 points equal to one another - just
				// join the line segment and bail out. This isn't an error, eg, could
				// be a 3d triangle that is edge on when projected
				if(xen::intersect(*line, viewport)){
					// :TODO: correct depth values here... not 0,0
					doRenderLine2d(target, *line, color, color, 0, 0);
				}
				return;
			}
		}

		// Sort Points such that p1 has lowest y, p3 has highest y
		if (tri.p1.y > tri.p2.y) { swap(tri.p1, tri.p2); }
		if (tri.p2.y > tri.p3.y) { swap(tri.p2, tri.p3); }
		if (tri.p1.y > tri.p2.y) { swap(tri.p1, tri.p2); }

		// :TODO: this is nasty hack to prevent infinite loop since we will
		// never step up in y if the line is along x - but we still want to
		// draw triangles
		// This is case like:
		//
		//       /|
		//      / |
		//     /  |
		//     ----   <- problem edge
		if((u32)tri.p1.y == (u32)tri.p2.y){
			return;
		}

		//printf("Points of triangle: (%f,%f), (%f,%f), (%f,%f)\n",
		//       tri.p1.x, tri.p1.y,
		//       tri.p2.x, tri.p2.y,
		//       tri.p3.x, tri.p3.y);

		// Create line for each of a, b, & c for drawing purposes
		xen::LineSegment2r line_a = {tri.p1, tri.p3};
		xen::LineSegment2r line_b = {tri.p1, tri.p2};
		xen::LineSegment2r line_c = {tri.p2, tri.p3};

		//////////////////////////////////////////////////////////
		// Find step vector for line_a as well as start position
		real num_pixels_a = xen::max(abs(line_a.p1.x - line_a.p2.x), abs(line_a.p1.y - line_a.p2.y));
		//printf("Number of pixels in line_a: %f\n", num_pixels_a);
		Vec2r delta_a = (line_a.p2 - line_a.p1) / num_pixels_a;
		if(delta_a.y > 1.0f){ // ensure we never step up by multiple y
			delta_a /= delta_a.y;
		}
		Vec2r curr_a  = line_a.p1;

		//////////////////////////////////////////////////////////
		// Find step vector for line_b as well as start position
		real num_pixels_b = xen::max(abs(line_b.p1.x - line_b.p2.x), abs(line_b.p1.y - line_b.p2.y));
		//printf("Number of pixels in line_b: %f\n", num_pixels_b);
		Vec2r delta_b = (line_b.p2 - line_b.p1) / num_pixels_b;
		if(delta_b.y > 1.0f){ // ensure we never step up by multiple y
			delta_b /= delta_b.y;
		}
		Vec2r curr_b  = line_b.p1;

		//////////////////////////////////////////////////////////
		// Find step vector for line_b, no start position as c is drawn after b
		// is finished -> IE: (line_c.start == line_b.end)
		real num_pixels_c = xen::max(abs(line_c.p1.x - line_c.p2.x), abs(line_c.p1.y - line_c.p2.y));
		//printf("Number of pixels in line_c: %f\n", num_pixels_c);
		Vec2r delta_c = (line_c.p2 - line_c.p1) / num_pixels_c;
	  if(delta_c.y > 1.0f){ // ensure we never step up by multiple y
			delta_c /= delta_c.y;
		}

	  //////////////////////////////////////////////////////////
		// Step along each line, connecting along x after each step in y
		u32 line_a_pixels_drawn = 0;
		while(line_a_pixels_drawn < num_pixels_a){ // :TODO: This may need to be just < or != etc, idek
			u32 curr_pixel_y = (u32)curr_a.y;
			XenAssert(curr_pixel_y == (u32)curr_b.y, "Expected current y values to be equal!");

			// Step along line a until we reach next y value
			while((u32)curr_a.y == curr_pixel_y){
				//printf("Stepping along a, now at: %f, %f\n", curr_a.x, curr_a.y);
				if (xen::contains(viewport, curr_a)){
					target.color[(u32)curr_a.y*target.width + (u32)curr_a.x] = color;
				}
				curr_a += delta_a;
				++line_a_pixels_drawn;
			}

			// Step along line b until we reach next y value
			while ((u32)curr_b.y == curr_pixel_y){
				//printf("Stepping along b, now at: %f, %f\n", curr_b.x, curr_b.y);
				if (xen::contains(viewport, curr_b)){
					target.color[(u32)curr_b.y*target.width + (u32)curr_b.x] = color;
				}
				curr_b += delta_b;
				// If b has reached end point then begin drawing line c by changing
				// vector that represents a single step from that of b to that of c
				if ((Vec2u)curr_b == (Vec2u)line_b.p2){
					delta_b = delta_c;
				}
			}

			//printf("curr_a.y: %f, curr_b.y: %f\n", curr_a.y, curr_b.y);
			XenAssert((u32)curr_a.y == (u32)curr_b.y, "Expected y of both currents to be equivalent");

			//printf("Val of curr_a: %f, Val of curr_b: %f\n", curr_a.x, curr_b.x);

			// draw horizontal line
			u32 min_x = xen::max(xen::min(curr_a.x, curr_b.x, viewport.max.x), viewport.min.x);
			u32 max_x = xen::min(xen::max(curr_a.x, curr_b.x, viewport.min.x), viewport.max.x);
			if(curr_a.y >= viewport.min.y && curr_a.y <= viewport.max.y){
				for (u32 x = min_x+1; x < max_x; ++x){
					target.color[(u32)curr_a.y*target.width + x] = color;
				}
			}
		}

	}

	void doRenderTriangle3d(xen::sren::RenderTargetImpl& target,
		                      const xen::Aabb2r& viewport,
												  const Mat4f& mvp_matrix,
	                        xen::Color4f color,
	                        xen::Triangle3r& tri){
		xen::Triangle4r tri_clip  = xen::toHomo(tri) * mvp_matrix;

		// Work out which points are behind the camera
		u08 points_behind = 0;
		points_behind |= (tri_clip.p1.z < 0) << 0;
		points_behind |= (tri_clip.p2.z < 0) << 1;
		points_behind |= (tri_clip.p3.z < 0) << 2;

		printf("Drawing triangle case %i\n", points_behind);

		///////////////////////////////////////////////////////////////////
		// Render the triangle(s)
		// Various cases depending on which of the points are behind the camera
		switch(points_behind){
		case 7: // 111 - all points behind camera, do nothing
			return;
		case 0: { // 000 - all points in front of camera
			// Do perspective divide (into normalized device coordinates -> [-1, 1]
			tri_clip.p1 /= (tri_clip.p1.w);
			tri_clip.p2 /= (tri_clip.p2.w);
			tri_clip.p3 /= (tri_clip.p3.w);

			xen::Triangle2r tri_screen =
				_convertToScreenSpace<xen::Triangle4r, xen::Triangle2r>(tri_clip, viewport);

			doRenderTriangle2d(target, viewport, color, tri_screen);
			return;
		}

		case 3:   // 011
			xen::swap(tri_clip.p1, tri_clip.p3);
			goto do_draw_2_behind;
		case 5:   // 101
			xen::swap(tri_clip.p1, tri_clip.p2);
			goto do_draw_2_behind;
		case 6:   // 110
		do_draw_2_behind: {
				printf("Doing draw 2 behind case\n");
				// There are two points behind the camera, so just make a new triangle
				// between the single point in front of the camera, and where the lines
				// intersect z = 0 plane.
				//
				// Move point p2 and p3 along the line connecting them to p1 such that z
				// component becomes 0
				Vec4r delta_p2 = (tri_clip.p1 - tri_clip.p2);
				tri_clip.p2 += (delta_p2 / delta_p2.z) * -tri_clip.p2.z;

				Vec4r delta_p3 = (tri_clip.p1 - tri_clip.p3);
				tri_clip.p3 += (delta_p3 / delta_p3.z) * -tri_clip.p3.z;

				// Do perspective divide (into normalized device coordinates -> [-1, 1]
				tri_clip.p1 /= (tri_clip.p1.w);
				tri_clip.p2 /= (tri_clip.p2.w);
				tri_clip.p3 /= (tri_clip.p3.w);

				xen::Triangle2r tri_screen =
					_convertToScreenSpace<xen::Triangle4r, xen::Triangle2r>(tri_clip, viewport);

				doRenderTriangle2d(target, viewport, color, tri_screen);
				return;
			}
		case 1: // 001
			xen::swap(tri_clip.p1, tri_clip.p3);
			goto do_draw_1_behind;
		case 2: // 010
			xen::swap(tri_clip.p2, tri_clip.p3);
			goto do_draw_1_behind;
		case 4: // 100
		do_draw_1_behind: {
				// If there is a single point behind the camera then we must generate 2
				// triangles to draw (we chop off a vertex of a triangle, generating a
				// quadrilateral - which we draw as two triangles)
				//
				// p3 is the point behind the camera, slide it to z = 0 along the lines
				// joining it to p1 and p2
				Vec4r delta_p1       = (tri_clip.p1 - tri_clip.p3);
				Vec4r p3_slide_to_p1 = (tri_clip.p3 + ((delta_p1 / delta_p1.z) * -tri_clip.p3.z));

				Vec4r delta_p2       = (tri_clip.p2 - tri_clip.p3);
				Vec4r p3_slide_to_p2 = (tri_clip.p3 + ((delta_p2 / delta_p2.z) * -tri_clip.p3.z));

				xen::VertexGroup2r<4> quad_screen;
				quad_screen.vertices[0] = (p3_slide_to_p1 / p3_slide_to_p1.w).xy;
				quad_screen.vertices[1] = (tri_clip.p1    / tri_clip.p1.w).xy;
				quad_screen.vertices[2] = (tri_clip.p2    / tri_clip.p2.w).xy;
				quad_screen.vertices[3] = (p3_slide_to_p2 / p3_slide_to_p2.w).xy;

				quad_screen =
					_convertToScreenSpace<xen::VertexGroup2r<4>, xen::VertexGroup2r<4>>(quad_screen, viewport);

				doRenderTriangle2d(target, viewport, color, *(xen::Triangle2r*)&quad_screen.vertices[0]);
				doRenderTriangle2d(target, viewport, color, *(xen::Triangle2r*)&quad_screen.vertices[1]);
			}
			return;
		}
		///////////////////////////////////////////////////////////////////
	}
}



namespace xen{

	namespace sren {
		void clear(xen::sren::RenderTargetImpl& target, Color color) {
			Color4f color01 = (Color4f)color;
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.color[i] = color01;
			}
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.depth[i] = FLT_MAX;
			}
		}

		void clear(RenderTargetImpl& target, const xen::Aabb2u& viewport, Color color){
			Color4f color01 = (Color4f)color;

			for(u32 y = viewport.min.y; y < viewport.max.y; ++y){
				// still need to stride by width of whole target to get to next line in
				// y, not just the width of the viewport
				u32 base = y * target.width;
				for(u32 x = viewport.min.x; x < viewport.max.x; ++x){
					target.color[base + x] = color01;
					target.depth[base + x] = FLT_MAX;
				}
			}
		}

		void renderRasterize(xen::sren::RenderTargetImpl& target, const xen::Aabb2u& viewport,
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands){

			// Find the actual view_region we wish to draw to. This is the
			// intersection of the actual target, and the user specified viewport
			xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
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

				// :TODO: support meshes
				if(cmd->geometry_source != xen::RenderCommand3d::IMMEDIATE){
					continue;
				}

				switch(cmd->primative_type){
				case xen::PrimativeType::POINTS:
					doRenderPoints(target, view_region, mat_mvp, cmd->immediate);
					break;
				case xen::PrimativeType::LINES:
					stride = 2;
					goto do_render_lines;
					break;
				case xen::PrimativeType::TRIANGLES:
					for(u32 i = 0; i < cmd->immediate.vertex_count - 1; i += 3){
						Triangle3r* tri_world = (Triangle3r*)(&cmd->immediate.position[i]);
						doRenderTriangle3d(target,view_region, mat_mvp, cmd->color, *tri_world);
					}
					break;
				case xen::PrimativeType::LINE_STRIP:
					stride = 1;
				do_render_lines:
					for(u32 i = 0; i < cmd->immediate.vertex_count - 1; i += stride){
						LineSegment3r* line_world = (LineSegment3r*)(&cmd->immediate.position[i]);

						// :TODO: In case where no colour is still specified we still lerp across each value between white and white, we may be able to optimise this

						// If (color is not specified) -> set to white, else -> get color for vertex
						xen::Color4f color1;
						if(&cmd->immediate.color == nullptr){
							color1 = xen::Color::WHITE4f;
						} else {
							color1 = makeColor4f(cmd->immediate.color[i]);
						}
						xen::Color4f color2;
						if(&cmd->immediate.color == nullptr){
							color1 = xen::Color::WHITE4f;
						} else {
							color2 = makeColor4f(cmd->immediate.color[i+1]);
						}
						// Multiply by cmd color, for case where colour for entire line is specified in command
						color1 *= cmd->color;
						color2 *= cmd->color;

						doRenderLine3d(target, view_region, mat_mvp, color1, color2, *line_world);
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
