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
#include <xen/core/array.hpp>

#include "renderer3d.hxx"
#include "RenderTargetImpl.hxx"

#include <cstring>
#include <cstdlib>
#include <float.h>

////////////////////////////////////////////////////////////////////////////////
//
// A note on spaces:
// -----------------
//
// In this file geometry is typically encoded in one of the following spaces
// - model
//   - positions relative to the origin of the model/mesh
// - world
//   - positions relative to the world's origin
// - clip
//   - something???
// - screen
//   - positions relative to the origin of the screen where x and y is pixel
//     on the screen, and z is the depth
//
// Variable names should be suffixed with _world, _clip etc to indicate the
// space that variable is in.
//
// Screen space is given in pixel coordinates, where x and y is the position
// and the screen
//
// Transformations between the spaces are defined as the following:
//
// model
//   |
//   | vector * model_matrix
//   v
// world
//   |
//   | vector * view_projection_matrix (vp_matrix)
//   v
// clip
//   |
//   | use the _convertToScreenSpace functions
//   v
// screen
////////////////////////////////////////////////////////////////////////////////

// :TODO: lighting calculations currently all wrong - we are passing in
// model space to the fragmentShader as world space

namespace {

	// :TODO: consolidate all of these convert to screen space functions into single
	// function
	template<typename T_IN, typename T_OUT>
  T_OUT _convertToScreenSpaceOLD(const T_IN& in_clip, const xen::Aabb2r& viewport){
		// :TODO: include depth information as z
		T_OUT out_screen = xen::swizzle<'x','y'>(in_clip);

		out_screen += Vec2r{1,1};                  // convert to [0, 2] space
		out_screen /= 2.0_r;                       // convert to [0, 1] space
		out_screen *= viewport.max - viewport.min; // convert to screen space
		out_screen += viewport.min;                // move to correct part of screen

		return out_screen;
	}

	Vec3r _convertToScreenSpace(const Vec4r in_clip, const::xen::Aabb2r& viewport){
		Vec3r out_screen = in_clip.xyz;

		// Do perspective divide such that things farther from camera are nearer in
		// screen space
		out_screen.xy /= in_clip.z;

		out_screen.xy += Vec2r{1,1};                  // [-1, 1] => [0, 2] space
		out_screen.xy /= 2.0_r;                       // [ 0, 2] => [0, 1] space
		out_screen.xy *= viewport.max - viewport.min; // [ 0, 1] => screen space (pixels)
		out_screen.xy += viewport.min;                // move to correct part of screen

		return out_screen;
	}


	xen::Triangle3r _convertToScreenSpaceTriOLD(const xen::Triangle4r& in_clip,
	                                            const xen::Aabb2r& viewport){
		xen::Triangle3r out_screen = xen::swizzle<'x','y','z'>(in_clip);

		out_screen += Vec3r{1,1,0};                                 // convert to [0, 2] space
		out_screen /= 2.0_r;                                        // convert to [0, 1] space
		out_screen *= xen::mkVec(viewport.max - viewport.min, 1_r); // convert to screen space
		out_screen += xen::mkVec(viewport.min,                0_r); // move to correct part of screen

		// Preserve z component of input
		out_screen.p1.z = in_clip.p1.z;
		out_screen.p2.z = in_clip.p2.z;
		out_screen.p3.z = in_clip.p3.z;

		return out_screen;
	}

	xen::Quad3r _convertToScreenSpaceQuadOLD(const xen::Quad3r in_clip,
		                                    const xen::Aabb2r& viewport){
		xen::Quad3r out_screen = xen::swizzle<'x','y','z'>(in_clip);

		out_screen += Vec3r{1,1,0};                                 // convert to [0, 2] space
		out_screen /= 2.0_r;                                        // convert to [0, 1] space
		out_screen *= xen::mkVec(viewport.max - viewport.min, 1_r); // convert to screen space
		out_screen += xen::mkVec(viewport.min,                0_r); // move to correct part of screen

		// Preserve z component of input
		out_screen.p1.z = in_clip.p1.z;
		out_screen.p2.z = in_clip.p2.z;
		out_screen.p3.z = in_clip.p3.z;
		out_screen.p4.z = in_clip.p4.z;

		return out_screen;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs fragment shader computations - IE: lighting
	/// \todo texture lookup
	/// \param params       The rendering parameters describing the whole scene
	/// \param pos_world    The position of the point being filled in world space
	/// \param normal_world The normal of the point being filled in world space
	/// \param color        The base diffuse color of the point being filled
	/////////////////////////////////////////////////////////////////////
	xen::Color4f _fragmentShader(const xen::RenderParameters3d& params,
	                             Vec3r                          pos_world,
	                             Vec3r                          normal_world,
	                             xen::Color4f                   color){

		xen::Color4f result = xen::Color4f::Origin;

		xen::Color3f total_light = params.ambient_light;

		for(u32 i = 0; i < xen::size(params.lights); ++i){
			if(params.lights[i].type != xen::LightSource3d::POINT){
				printf("WARN: Unsupported light type in rasterizer\n");
				continue;
			}

			real dist_sq_world = xen::distanceSq(pos_world, params.lights[i].point.position);

			total_light += xen::sren::computeLightInfluence
				(params.lights[i].color,
				 params.lights[i].attenuation,
				 dist_sq_world
				);
		}

		for(u32 i = 0; i < 3; ++i){
			if(total_light.elements[i] > 1){
				total_light.elements[i] = 1;
			}
		}

		result.rgb = color.rgb * total_light;
		result.a   = 1;

		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Renders a set of points in world space to the screen
	/// \param target       The render target to draw on
	/// \param params       Render parameters describing the scene as a whole
	/// \param viewport     The viewport of the target that may be drawn to
	/// \param m_matrix     Model matrix representing transform from model space
	///                     to world space
	/// \param vp_matrix    View-projection matrix representing transform from
	///                     world space to clip space
	/// \param base_color   The base color of points
	/// \param pos_model    The positions of the points to render in model space
	///                     Array length should equal vertex_count
	/// \param color_buffer Array of per vertex colors. May be nullptr in which
	///                     case the base_color will be used unmodified
	/// \param vertex_count Number of vertices to draw
	/////////////////////////////////////////////////////////////////////
	void _renderPointsWorld(xen::sren::RenderTargetImpl&        target,
	                        const xen::RenderParameters3d&      params,
	                        const xen::Aabb2r&                  viewport,
	                        const Mat4r&                        m_matrix,
	                        const Mat4r&                        vp_matrix,
	                        const xen::Color4f                  color,
	                        const Vec3r*                        pos_model,
	                        const xen::Color*                   color_buffer,
	                        const u32                           vertex_count){

		bool colors_per_vertex = (color_buffer != nullptr);
		if(!colors_per_vertex){
			color_buffer = &xen::Color::WHITE;
		}

		for(u32 i = 0; i < vertex_count; ++i){
			Vec4r point_world = xen::toHomo(pos_model[i]) * m_matrix;
			Vec4r point_clip  = point_world * vp_matrix;

			if(point_clip.x < -point_clip.w ||
			   point_clip.x >  point_clip.w ||
			   point_clip.y < -point_clip.w ||
			   point_clip.y >  point_clip.w ||
			   point_clip.z < -point_clip.w ||
			   point_clip.z >  point_clip.w){
				// Then point is not in view of the camera
				continue;
			}

			Vec3r point_screen = _convertToScreenSpace(point_clip, viewport);

			u32 pixel_index = (u32)point_screen.y*target.width + (u32)point_screen.x;

			if (point_screen.z > target.depth[pixel_index]){
				// Then point is behind something else occupying this pixel
				continue;
			}

			target.depth[pixel_index] = point_clip.z;
			target.color[pixel_index] = _fragmentShader
				(params,
				 point_world.xyz,
				 Vec3r::Origin,
				 xen::makeColor4f(color_buffer[i * colors_per_vertex]) * color
				);
		}
	}

	void _renderLineWorld(xen::sren::RenderTargetImpl&  target,
	                      const xen::Aabb2r&            viewport,
	                      const xen::RenderParameters3d params,
	                      const Mat4r&                  mvp_matrix,
	                      const xen::LineSegment4f&     line_color,
	                      const xen::LineSegment3r&     line_world){

		xen::LineSegment4r line_clip  = xen::toHomo(line_world) * mvp_matrix;

		///////////////////////////////////////////////////////////////////
		// Do line clipping against z planes
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

		real z_start = line_clip.p2.z;
		real z_end   = line_clip.p1.z;

		///////////////////////////////////////////////////////////////////
		// Do perspective divide (into normalized device coordinates -> [-1, 1]
		// We only care about x and y coordinates at this point
		line_clip.p1.xy /= (line_clip.p1.w);
		line_clip.p2.xy /= (line_clip.p2.w);
		///////////////////////////////////////////////////////////////////

		///////////////////////////////////////////////////////////////////
		// Clip to the viewport
		xen::LineSegment2r line_screen =
			_convertToScreenSpaceOLD<xen::LineSegment4r, xen::LineSegment2r>(line_clip, viewport);

		if(!xen::intersect(line_screen, viewport)){
			return;
		}
		if(line_screen.p1 == line_screen.p2){ return; }
		///////////////////////////////////////////////////////////////////


		//https://www.cs.virginia.edu/luther/blog/posts/492.html
		real num_pixels = xen::max(abs(line_screen.p1.x - line_screen.p2.x),
		                           abs(line_screen.p1.y - line_screen.p2.y)
		                          );

		Vec2r delta = (line_screen.p1 - line_screen.p2) / num_pixels;
		Vec2r cur   = line_screen.p2;
		for(u32 i = 0; i < (u32)num_pixels; ++i){

			// :TODO: Replace lerp with a perspective correct interpolation
			real lerp_factor = i / num_pixels;
			u32  pixel_index = (u32)cur.y*target.width + cur.x;

			real         depth      = xen::lerp(z_start, z_end, lerp_factor);
			xen::Color4f base_color = xen::lerp(line_color,     lerp_factor);
			Vec3r        pos_world  = xen::lerp(line_world,     lerp_factor);

			if (depth < target.depth[pixel_index]) {
				target.color[pixel_index] = _fragmentShader(params,
				                                            pos_world,
				                                            Vec3r::Origin,
				                                            base_color
				                                           );
				target.depth[pixel_index] = depth;
			}
			cur += delta;
		}

	}

	void _renderTriangleScreen(xen::sren::RenderTargetImpl&   target,
	                           const xen::Aabb2r&             viewport,
	                           const xen::RenderParameters3d& params,
	                           xen::Triangle3r                tri_world,
	                           xen::Triangle3r                tri_screen,
	                           xen::Triangle4f                colors){
		xen::Triangle2r tri_screen_xy = xen::swizzle<'x','y'>(tri_screen);

		xen::Aabb2r region_r = xen::Aabb2r::MaxMinBox;
		xen::addPoint(region_r, tri_screen_xy.p1);
		xen::addPoint(region_r, tri_screen_xy.p2);
		xen::addPoint(region_r, tri_screen_xy.p3);
		if(!xen::intersect(region_r, viewport)){
			return;
		}
		xen::Aabb2u region = (xen::Aabb2u)region_r;

		// If we call min.x, 0 and max.x, 1 then incr_x is the amount we increase
		// by when we move 1 pixel
		real incr_x = 1.0_r / (real)(region.max.x - region.min.x);
		real incr_y = 1.0_r / (real)(region.max.y - region.min.y);

		// Barycentric coordinates vary as a lerp along some axis, hence rather
		// than computing the barycentric coordinate at each position in the Aabb
		// of the triangle we can compute it at the corners and then lerp
		// across
		Vec3r bary_bottom_left  = xen::getBarycentricCoordinates(tri_screen_xy, Vec2r{
				(real)region.min.x, (real)region.min.y});
		Vec3r bary_bottom_right = xen::getBarycentricCoordinates(tri_screen_xy, Vec2r{
				(real)region.max.x, (real)region.min.y});
		Vec3r bary_top_left     = xen::getBarycentricCoordinates(tri_screen_xy, Vec2r{
				(real)region.min.x, (real)region.max.y});
		Vec3r bary_top_right    = xen::getBarycentricCoordinates(tri_screen_xy, Vec2r{
				(real)region.max.x, (real)region.max.y});

		float frac_y = 0.0_r;
		for(u32 y = region.min.y; y <= region.max.y; ++y, frac_y += incr_y){
			u32 pixel_index_base = y*target.width;

			Vec3r bary_left  = xen::lerp(bary_bottom_left,  bary_top_left,  frac_y);
			Vec3r bary_right = xen::lerp(bary_bottom_right, bary_top_right, frac_y);

			// A pixel is in the triangle if all components of the barycentric
			// coordinate is positive. We could loop between region.min.x and
			// region.max.x, lerp out the bary centric coordinate and compute
			// if the pixel is "in" on a per pixel basis.
			// However since each component of the barycentric coordinate varies
			// linearly with frac_x we can compute the point where the sign
			// of each component flips, and constrain the range of x values that
			// we loop over to be just those within the triangle

			real min_x = 0.0_r;
			real max_x = 1.0_r;

			bool valid_row = true;

			// :TODO: we are doing these instructions 3 wide (but branching)
			// can we use simd?
			for(u32 i = 0; i < 3; ++i){
				if(bary_left[i] >= 0 && bary_right[i] >= 0){
					// Then this component is always positive, we do not need to
					// constrain the x range based on it
					continue;
				} else if (bary_left[i] < 0 && bary_right[i] < 0){
					// Then this component is always negative, there is nothing
					// to draw on this y
					//
					// :TODO: why does this ever happen?
					//
					// Current guess (no evidence):
					// ----------------------------
					// This probably happens when the bounding box of the triangle has
					// been clipped to the viewport such that there are no pixels
					// in the triangle for some y vale
					//
					// Can we do clipping to viewport in smarter way to avoid this?
					valid_row = false;
					break;
				} else if(bary_left[i] < 0 && bary_right[i] >= 0) {
					// Then the first n pixels of the row are not in the triangle (until
					// we reach the point where the component hits 0).

					// Compute where this cross over point is:
					real sign_flip = -bary_left[i] / (bary_right[i] - bary_left[i]);

					// Update min_x if need be
					min_x = xen::max(min_x, sign_flip);
				} else { //(bary_left[i] >= 0 && bary_right[i] < 0)
					// Then the last n pixels of the row are not in the triangle (after
					// we reach the point where the component hits 0).

					// Compute where this cross over point is:
					real sign_flip = bary_left[i] / (bary_left[i] - bary_right[i]);

					// Update max_x if need be
					max_x = xen::min(max_x, sign_flip);
				}
			}

			if(!valid_row){ continue; }

			// Now fill pixels between min and max
			real frac_x = min_x;
			for(u32 x  = xen::lerp(region.min.x, region.max.x, min_x);
			    x     <= xen::lerp(region.min.x, region.max.x, max_x);
			    ++x, frac_x += incr_x
			    ){

				Vec3r bary = xen::lerp(bary_left, bary_right, frac_x);

				u32 pixel_index = pixel_index_base + x;

				xen::Color4f color        = evaluateBarycentricCoordinates(colors,     bary);
				Vec3r        depth        = evaluateBarycentricCoordinates(tri_screen, bary);
				Vec3r        pos_world    = evaluateBarycentricCoordinates(tri_world,  bary);
				Vec3r        normal_world = Vec3r::Origin; // :TODO: compute

				if (depth.z < target.depth[pixel_index]) {
					target.depth[pixel_index] = depth.z;
					target.color[pixel_index] = _fragmentShader(params,
					                                            pos_world,
					                                            normal_world,
					                                            color);
				}
			}
		}
	}

	void _renderTriangleWorld(xen::sren::RenderTargetImpl&  target,
		                      const xen::Aabb2r&            viewport,
	                        const xen::RenderParameters3d params,
												  const Mat4r&                  mvp_matrix,
	                        const xen::Triangle3r&        tri_world,
	                        xen::Triangle4f               tri_color){
		xen::Triangle4r tri_clip  = xen::toHomo(tri_world) * mvp_matrix;

		// Work out which points are behind the camera
		u08 points_behind = 0;
		points_behind |= (tri_clip.p1.z < 0) << 0;
		points_behind |= (tri_clip.p2.z < 0) << 1;
		points_behind |= (tri_clip.p3.z < 0) << 2;

		///////////////////////////////////////////////////////////////////
		// Render the triangle(s)
		// Various cases depending on which of the points are behind the camera
		switch(points_behind){
		case 7: // 111 - all points behind camera, do nothing
			return;
		case 0: { // 000 - all points in front of camera
			// Do perspective divide (into normalized device coordinates -> [-1, 1]
			tri_clip.p1.xy /= (tri_clip.p1.w);
			tri_clip.p2.xy /= (tri_clip.p2.w);
			tri_clip.p3.xy /= (tri_clip.p3.w);

			xen::Triangle3r tri_screen =
				_convertToScreenSpaceTriOLD(tri_clip, viewport);

			_renderTriangleScreen(target, viewport, params,
			                      tri_world, tri_screen, tri_color);
			return;
		}

		case 3:   // 011
			xen::swap(tri_clip.p1,  tri_clip.p3 );
			xen::swap(tri_color.p1, tri_color.p3);
			goto do_draw_2_behind;
		case 5:   // 101
			xen::swap(tri_clip.p1,  tri_clip.p2 );
			xen::swap(tri_color.p1, tri_color.p2);
			goto do_draw_2_behind;
		case 6:   // 110
		do_draw_2_behind: {
				// There are two points behind the camera, so just make a new triangle
				// between the single point in front of the camera, and where the lines
				// intersect z = 0 plane.
				//
				// Move point p2 and p3 along the line connecting them to p1 such that z
				// component becomes 0

				Vec4r delta_p2 = (tri_clip.p1 - tri_clip.p2);
				real  frac_p2  = ((-tri_clip.p2.z / delta_p2.z));
				tri_clip.p2   += delta_p2 * frac_p2;
				tri_color.p2  += (tri_color.p1 - tri_color.p2) * frac_p2;

				Vec4r delta_p3 = (tri_clip.p1 - tri_clip.p3);
				real  frac_p3  = ((-tri_clip.p3.z / delta_p3.z));
				tri_clip.p3   += delta_p3 * frac_p3;
				tri_color.p3  += (tri_color.p1 - tri_color.p3) * frac_p3;

				// Do perspective divide (into normalized device coordinates -> [-1, 1]
				tri_clip.p1.xy /= (tri_clip.p1.w);
				tri_clip.p2.xy /= (tri_clip.p2.w);
				tri_clip.p3.xy /= (tri_clip.p3.w);

				xen::Triangle3r tri_screen = _convertToScreenSpaceTriOLD(tri_clip, viewport);

				_renderTriangleScreen(target, viewport, params,
				                      tri_world, tri_screen, tri_color);
				return;
			}
		case 1: // 001
			xen::swap(tri_clip.p1,  tri_clip.p3 );
			xen::swap(tri_color.p1, tri_color.p3);
			goto do_draw_1_behind;
		case 2: // 010
			xen::swap(tri_clip.p2,  tri_clip.p3 );
			xen::swap(tri_color.p2, tri_color.p3);
			goto do_draw_1_behind;
		case 4: // 100
		do_draw_1_behind: {
				// If there is a single point behind the camera then we must generate 2
				// triangles to draw (we chop off a vertex of a triangle, generating a
				// quadrilateral - which we draw as two triangles)
				//
				// p3 is the point behind the camera, slide it to z = 0 along the lines
				// joining it to p1 and p2
				//Vec4r delta_p3 = (tri_clip.p1 - tri_clip.p3);
				//tri_clip.p3 += delta_p3 * ((-tri_clip.p3.z / delta_p3.z));

				Vec4r delta_p1       = (tri_clip.p1 - tri_clip.p3);
				real  frac_p1        = ((-tri_clip.p3.z / delta_p1.z));
				Vec4r p3_slide_to_p1 = (tri_clip.p3 + (delta_p1 * frac_p1));

				Vec4r delta_p2       = (tri_clip.p2 - tri_clip.p3);
				real  frac_p2        = ((-tri_clip.p3.z / delta_p2.z));
				Vec4r p3_slide_to_p2 = (tri_clip.p3 + (delta_p2 * frac_p2));

				xen::VertexGroup3r<4> quad_screen;
				tri_clip.p1.xy /= (tri_clip.p1.w);
				quad_screen.vertices[0] = tri_clip.p1.xyz;
			  p3_slide_to_p1.xy /= p3_slide_to_p1.w;
				quad_screen.vertices[1] = p3_slide_to_p1.xyz;
				tri_clip.p2.xy /= tri_clip.p2.w;
				quad_screen.vertices[2] = tri_clip.p2.xyz;
				p3_slide_to_p2.xy /= p3_slide_to_p2.w;
				quad_screen.vertices[3] = p3_slide_to_p2.xyz;

				xen::VertexGroup3r<4> quad_world;
				quad_world.vertices[0] = tri_world.p1;
				quad_world.vertices[1] = tri_world.p3 + (tri_world.p1 - tri_world.p3) * frac_p1;
				quad_world.vertices[2] = tri_world.p2;
				quad_world.vertices[3] = tri_world.p3 + (tri_world.p2 - tri_world.p3) * frac_p2;

				xen::VertexGroup4f<4> quad_colors;
				quad_colors.vertices[0] = tri_color.p1;
				quad_colors.vertices[1] = tri_color.p3 + (tri_color.p1 - tri_color.p3) * frac_p1;
				quad_colors.vertices[2] = tri_color.p2;
				quad_colors.vertices[3] = tri_color.p3 + (tri_color.p2 - tri_color.p3) * frac_p2;

				quad_screen = _convertToScreenSpaceQuadOLD(quad_screen, viewport);

				_renderTriangleScreen(target, viewport, params,
				                      *(xen::Triangle3r*)&quad_world.vertices [0],
				                      *(xen::Triangle3r*)&quad_screen.vertices[0],
				                      *(xen::Triangle4f*)&quad_colors.vertices[0]
				                     );
				_renderTriangleScreen(target, viewport, params,
				                      *(xen::Triangle3r*)&quad_world.vertices [1],
				                      *(xen::Triangle3r*)&quad_screen.vertices[1],
				                      *(xen::Triangle4f*)&quad_colors.vertices[1]
				                     );
			}
			return;
		}
		///////////////////////////////////////////////////////////////////
	}
}

namespace xen{

	namespace sren {
		void renderRasterize(xen::sren::RenderTargetImpl& target, const xen::Aabb2u& viewport,
		                     const RenderParameters3d& params,
		                     const xen::Array<RenderCommand3d>& commands){

			#ifdef XEN_DEBUG_ADDITIONAL_CHECKS
			if(!xen::isCameraValid(params.camera)){
				printf("ERROR: Camera is not valid, skipping rendering\n");
				return;
			}
			#endif

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

				#ifdef XEN_DEBUG_ADDITIONAL_CHECKS
				if(xen::isnan(mat_mvp)){
					printf("ERROR: MVP matrix contains NaN elements, skipping command %i\n",
					       cmd_index);
					continue;
				}
				#endif

				Color4f base_color = cmd->color;
				base_color.rgb *= params.ambient_light;

				// :TODO: support meshes
				if(cmd->geometry_source != xen::RenderCommand3d::IMMEDIATE){
					continue;
				}

				switch(cmd->primitive_type){
				case xen::PrimitiveType::POINTS:
					_renderPointsWorld(target, params, view_region,
					                   cmd->model_matrix, mat_vp,
					                   cmd->color,
					                   cmd->immediate.position, cmd->immediate.color,
					                   cmd->immediate.vertex_count);
					break;
				case xen::PrimitiveType::LINES:
					stride = 2;
					goto do_render_lines;
					break;
				case xen::PrimitiveType::LINE_STRIP:
					stride = 1;
				do_render_lines:
					for(u32 i = 0; i < cmd->immediate.vertex_count - 1; i += stride){
						LineSegment3r* line_world = (LineSegment3r*)(&cmd->immediate.position[i]);
						LineSegment4f  line_color = { cmd->color, cmd->color };

						if(cmd->immediate.color != nullptr){
							line_color.p1 *= xen::makeColor4f(cmd->immediate.color[i+0]);
							line_color.p2 *= xen::makeColor4f(cmd->immediate.color[i+1]);
						}

						_renderLineWorld(target, view_region, params,
						                 mat_mvp, line_color, *line_world
						                 );
					}
					break;
				case xen::PrimitiveType::TRIANGLES: {
					for(u32 i = 0; i < cmd->immediate.vertex_count - 1; i += 3){
						Triangle3r* tri_world = (Triangle3r*)(&cmd->immediate.position[i]);

						xen::Triangle4f tri_color = {
							cmd->color,
							cmd->color,
							cmd->color,
						};

						if(cmd->immediate.color != nullptr){
							tri_color.p1 *= makeColor4f(cmd->immediate.color[i+0]);
							tri_color.p2 *= makeColor4f(cmd->immediate.color[i+1]);
							tri_color.p3 *= makeColor4f(cmd->immediate.color[i+2]);
						}

						_renderTriangleWorld(target, view_region, params,
						                     mat_mvp, *tri_world, tri_color
						                    );
					}

					break;
				}
				default:
					XenInvalidCodePath("Unhandled render command type in software rasterizer");
					break;
				}
			}
		}
	}
}

#endif
