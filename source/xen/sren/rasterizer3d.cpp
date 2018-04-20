////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for 3d rasterization
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_SREN_RASTERIZER3D_CPP
#define XEN_GRAPHICS_SREN_RASTERIZER3D_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/vertex_group.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/core/array.hpp>

#include <xen/sren/FragmentShader.hpp>
#include "rasterizer3d.hxx"
#include "render-utilities.hxx"
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

	Vec3r _convertToScreenSpace(const Vec4r in_clip,
	                            const::xen::Aabb2r& viewport){
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


	template<u32 T_NUM_VERTS>
	xen::VertexGroup3r<T_NUM_VERTS> _convertToScreenSpace(const xen::VertexGroup4r<T_NUM_VERTS> in_clip,
	                                                      const xen::Aabb2r& viewport){
		xen::VertexGroup3r<T_NUM_VERTS> out_screen;

		for(u32 i = 0; i < 0; ++i){
			out_screen.vertices[i] = _convertToScreenSpace(in_clip.vertices[i], viewport);
		}

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

	xen::Quad3r _convertToScreenSpaceQuadOLD(const xen::Quad4r in_clip,
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

void _rasterizeLineScreen(const xen::sren::RasterizationContext& cntx,
                          const xen::LineSegment3r&        line_world,
                          xen::LineSegment3r               line_screen,
                          const xen::LineSegment4f&        line_color){

	///////////////////////////////////////////////////////////////////
	// Slide the endpoints into the viewport
	xen::Aabb3r view_volumn;
	view_volumn.min.xy = cntx.viewport->min;
	view_volumn.min.z  = FLT_MIN;
	view_volumn.max.xy = cntx.viewport->max;
	view_volumn.max.z  = FLT_MAX;
	if(!xen::intersect(line_screen, view_volumn)){
		return;
	}
	if(line_screen.p1.xy == line_screen.p2.xy){ return; }
	///////////////////////////////////////////////////////////////////

	//https://www.cs.virginia.edu/luther/blog/posts/492.html
	real num_pixels = xen::max(abs(line_screen.p1.x - line_screen.p2.x),
	                           abs(line_screen.p1.y - line_screen.p2.y)
	                           );

	Vec3r delta_screen = (line_screen.p2 - line_screen.p1) / num_pixels;
	Vec3r cur_screen   = line_screen.p1;
	for(u32 i = 0; i < (u32)num_pixels; ++i){

		// :TODO: Replace lerp with a perspective correct interpolation
		real lerp_factor = i / num_pixels;

		// :TODO: should be u32 -> but cur.y or cur.z may be negative
		s32  pixel_index = (s32)cur_screen.y*cntx.target->width + cur_screen.x;

		real         depth      = cur_screen.z;
		xen::Color4f base_color = xen::lerp(line_color,     lerp_factor);
		Vec3r        pos_world  = xen::lerp(line_world,     lerp_factor);

		if (depth < cntx.target->depth[pixel_index]) {
			cntx.target->color[pixel_index] = cntx.fragment_shader
				(cntx, pos_world, Vec3r::Origin, base_color, Vec2f::Origin);
			cntx.target->depth[pixel_index] = depth;
		}
		cur_screen += delta_screen;
	}
}

	// :TODO:OPT: profile -> is passing by reference faster
	void _rasterizeTriangleScreen(const xen::sren::RasterizationContext& cntx,
	                              xen::Triangle3r                        tri_world,
	                              xen::Triangle3r                        tri_normal_world,
	                              xen::Triangle3r                        tri_screen,
	                              xen::Triangle4f                        colors,
	                              xen::Triangle2f                        tri_uvs){

		#if 0 // render wireframe of triangle
		_rasterizeLineScreen(cntx,
		                     *(xen::LineSegment3r*)&tri_world.p1,
		                     *(xen::LineSegment3r*)&tri_screen.p1,
		                     *(xen::LineSegment4f*)&colors.p1
		                    );
		_rasterizeLineScreen(cntx,
		                     *(xen::LineSegment3r*)&tri_world.p2,
		                     *(xen::LineSegment3r*)&tri_screen.p2,
		                     *(xen::LineSegment4f*)&colors.p2
		                    );

		xen::LineSegment3r line_world;
		xen::LineSegment3r line_screen;
		xen::LineSegment4f line_colors;
		line_world.p1 = tri_world.p1;
		line_world.p2 = tri_world.p3;
		line_screen.p1 = tri_screen.p1;
		line_screen.p2 = tri_screen.p3;
		line_colors.p1 = colors.p1;
		line_colors.p2 = colors.p3;
		_rasterizeLineScreen(cntx, line_world, line_screen, line_colors);
		return;
		#endif

		xen::Triangle2r tri_screen_xy = xen::swizzle<'x','y'>(tri_screen);

		xen::Aabb2r region_r = xen::Aabb2r::MaxMinBox;
		xen::addPoint(region_r, tri_screen_xy.p1);
		xen::addPoint(region_r, tri_screen_xy.p2);
		xen::addPoint(region_r, tri_screen_xy.p3);
		if(!xen::intersect(region_r, *cntx.viewport)){
			return;
		}

		// Divide for perspective correct interpolation
		// https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
		tri_world.p1        /= tri_screen.p1.z;
		tri_world.p2        /= tri_screen.p2.z;
		tri_world.p3        /= tri_screen.p3.z;
		tri_normal_world.p1 /= tri_screen.p1.z;
		tri_normal_world.p2 /= tri_screen.p2.z;
		tri_normal_world.p3 /= tri_screen.p3.z;
		tri_uvs.p1          /= (float)tri_screen.p1.z;
		tri_uvs.p2          /= (float)tri_screen.p2.z;
		tri_uvs.p3          /= (float)tri_screen.p3.z;
		colors.p1           /= (float)tri_screen.p1.z;
		colors.p2           /= (float)tri_screen.p2.z;
		colors.p3           /= (float)tri_screen.p3.z;
		tri_screen.p1.z = 1_r / tri_screen.p1.z;
		tri_screen.p2.z = 1_r / tri_screen.p2.z;
		tri_screen.p3.z = 1_r / tri_screen.p3.z;

		// Edge function method, see:
		// https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/rasterization-stage
		#if 1

		const real area = (((tri_screen.p3.x - tri_screen.p1.x) *
		              (tri_screen.p2.y - tri_screen.p1.y)
		              ) -
		             ((tri_screen.p3.y - tri_screen.p1.y) *
		              (tri_screen.p2.x - tri_screen.p1.x)
		              )
		             );

		for(u32 py = floor(region_r.min.y); py < ceil(region_r.max.y); ++py){
			for(u32 px = floor(region_r.min.x); px < ceil(region_r.max.x); ++px){


				real e12 = (((px -              tri_screen.p1.x) *
				             (tri_screen.p2.y - tri_screen.p1.y)
				            ) -
				            ((py              - tri_screen.p1.y) *
				             (tri_screen.p2.x - tri_screen.p1.x)
				            )
				           );
				real e23 = (((px -              tri_screen.p2.x) *
				             (tri_screen.p3.y - tri_screen.p2.y)
				            ) -
				            ((py              - tri_screen.p2.y) *
				             (tri_screen.p3.x - tri_screen.p2.x)
				            )
				           );
				real e31 = (((px -              tri_screen.p3.x) *
				             (tri_screen.p1.y - tri_screen.p3.y)
				            ) -
				            ((py              - tri_screen.p3.y) *
				             (tri_screen.p1.x - tri_screen.p3.x)
				            )
				           );

				if(! ((e12 > 0 && e23 > 0 && e31 > 0) ||
				      (e12 < 0 && e23 < 0 && e31 < 0)
				     )
				  ){
					// Then this point is outside of the triangle
					continue;
				}

				u32 pixel_index = py * cntx.target->width + px;

				Vec3r bary;
				bary.z = e12 / area;
				bary.x = e23 / area;
				bary.y = e31 / area;

				real depth = 1_r / ( tri_screen.p1.z * bary.x +
				                     tri_screen.p2.z * bary.y +
				                     tri_screen.p3.z * bary.z );

				xen::Color4f color        = evaluateBarycentricCoordinates(colors,           bary);
				Vec3r        pos_world    = evaluateBarycentricCoordinates(tri_world,        bary);
				Vec3r        normal_world = evaluateBarycentricCoordinates(tri_normal_world, bary);
				Vec2f        uvs          = evaluateBarycentricCoordinates(tri_uvs,          bary);

				// Correct for perspective correct interpolation
				// https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
				color        *= (float)depth;
				uvs          *= (float)depth;
				pos_world    *= depth;
				normal_world *= depth;

				if (depth < cntx.target->depth[pixel_index]) {
					cntx.target->depth[pixel_index] = depth;
					cntx.target->color[pixel_index] = cntx.fragment_shader
						(cntx, pos_world, normal_world, color, uvs);
				}
			}
		}

		// This is the made up barycentric interpolation method,
		// broken in some cases...
		#else
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
			u32 pixel_index_base = y * cntx.target->width;

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

			  real depth = 1_r / ( tri_screen.p1.z * bary.x +
				                     tri_screen.p2.z * bary.y +
				                     tri_screen.p3.z * bary.z );

				xen::Color4f color        = evaluateBarycentricCoordinates(colors,           bary);
				Vec3r        pos_world    = evaluateBarycentricCoordinates(tri_world,        bary);
				Vec3r        normal_world = evaluateBarycentricCoordinates(tri_normal_world, bary);

				// Correct for perspective correct interpolation
				// https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
				color        *= (float)depth;
				uvs          *= (float)depth;
				pos_world    *= depth;
				normal_world *= depth;

				if (depth < cntx.target->depth[pixel_index]) {
					cntx.target->depth[pixel_index] = depth;
					cntx.target->color[pixel_index] = cntx.fragment_shader
						(cntx, pos_world, normal_world, color, uvs);
				}
			}
		}
		#endif
	}
} // end of anon namespace

namespace xen {
namespace sren {

void setPerCommandFragmentUniforms(FragmentUniforms& uniforms,
                                   const Material&   material,
                                   const Mat4r&      m_mat,
                                   const Mat4r&      vp_mat){
	uniforms.m_matrix           = m_mat;
	uniforms.vp_matrix          = vp_mat;
	uniforms.diffuse_color      = material.color;
	uniforms.emissive_color     = material.emissive_color;
	uniforms.specular_exponent  = material.specular_exponent;
	uniforms.specular_intensity = material.specular_intensity;
}

void rasterizePointsModel(const RasterizationContext& cntx,
                          const Vec3r*                pos_model,
                          const Color*                color_buffer,
                          const u32                   vertex_count){

	bool colors_per_vertex = (color_buffer != nullptr);
	if(!colors_per_vertex){
		color_buffer = &xen::Color::WHITE;
	}

	for(u32 i = 0; i < vertex_count; ++i){
		Vec4r point_world = xen::toHomo(pos_model[i]) * cntx.m_matrix;
		Vec4r point_clip  = point_world * cntx.vp_matrix;

		if(point_clip.x < -point_clip.w ||
		   point_clip.x >  point_clip.w ||
		   point_clip.y < -point_clip.w ||
		   point_clip.y >  point_clip.w ||
		   point_clip.z < -point_clip.w ||
		   point_clip.z >  point_clip.w){
			// Then point is not in view of the camera
			continue;
		}

		Vec3r point_screen = _convertToScreenSpace(point_clip, *cntx.viewport);

		u32 pixel_index = (u32)point_screen.y*cntx.target->width + (u32)point_screen.x;

		if (point_screen.z > cntx.target->depth[pixel_index]){
			// Then point is behind something else occupying this pixel
			continue;
		}

		cntx.target->depth[pixel_index] = point_clip.z;
		cntx.target->color[pixel_index] = cntx.fragment_shader
			(cntx,
			 point_world.xyz,
			 Vec3r::Origin,
			 xen::makeColor4f(color_buffer[i * colors_per_vertex]),
			 Vec2f::Origin);
	}
} // end of rasterizePointsModel

void rasterizeLineModel(const RasterizationContext& cntx,
                        const LineSegment3r&        line_model,
                        const LineSegment4f&        line_color){

	xen::LineSegment3r line_world = line_model * cntx.m_matrix;
	xen::LineSegment4r line_clip  = xen::toHomo(line_world) * cntx.vp_matrix;

	///////////////////////////////////////////////////////////////////
	// Do line clipping against near z plane
	// :TODO: this is currently against z = 0, not z_near
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
	line_clip.p1.xy /= (line_clip.p1.z);
	line_clip.p2.xy /= (line_clip.p2.z);
	///////////////////////////////////////////////////////////////////

	///////////////////////////////////////////////////////////////////
	// Clip to the viewport
	xen::LineSegment2r line_screen_2d =
		_convertToScreenSpaceOLD<xen::LineSegment4r, xen::LineSegment2r>(line_clip, *cntx.viewport);

	xen::LineSegment3r line_screen;
	line_screen.p1.xy = line_screen_2d.p1;
	line_screen.p1.z  = line_clip.p1.z;
	line_screen.p2.xy = line_screen_2d.p2;
	line_screen.p2.z  = line_clip.p2.z;

	_rasterizeLineScreen(cntx, line_world, line_screen, line_color);
} // end of function rasterizeLineModel

void rasterizeLinesModel(const RasterizationContext& cntx,
                         const Vec3r*                pos_model,
                         const Color*                color_buffer,
                         const u32                   vertex_count,
                         const u32                   stride){
	for(u32 i = 0; i < vertex_count - 1; i += stride){
		LineSegment3r* line_model = (LineSegment3r*)(&pos_model[i]);
		LineSegment4f  line_color = { xen::Color::WHITE4f, xen::Color::WHITE4f };
		if(color_buffer != nullptr){
			line_color.p1 = xen::makeColor4f(color_buffer[i+0]);
			line_color.p2 = xen::makeColor4f(color_buffer[i+1]);
		}

		rasterizeLineModel(cntx, *line_model, line_color);
	}
} // end of rasterize lines model

// :TODO:OPT: profile -> is passing by reference faster?
void rasterizeTriangleModel(const RasterizationContext& cntx,
                            Triangle3r                  tri_pos_model,
                            Triangle3r                  tri_nor_model,
                            Triangle4f                  tri_color,
                            Triangle2f                  tri_uvs
                           ){

	xen::Triangle3r tri_pos_world = tri_pos_model * cntx.m_matrix;
	xen::Triangle3r tri_nor_world = tri_nor_model * cntx.m_matrix;

	// Model matrix may change scale of normals - re-normalise
	tri_nor_world.p1 = xen::normalized(tri_nor_world.p1);
	tri_nor_world.p2 = xen::normalized(tri_nor_world.p2);
	tri_nor_world.p3 = xen::normalized(tri_nor_world.p3);

	xen::Triangle4r tri_pos_clip = xen::toHomo(tri_pos_world) * cntx.vp_matrix;

	// Work out which points are behind the camera
	u08 points_behind = 0;
	points_behind |= (tri_pos_clip.p1.z < cntx.camera.z_near) << 0;
	points_behind |= (tri_pos_clip.p2.z < cntx.camera.z_near) << 1;
	points_behind |= (tri_pos_clip.p3.z < cntx.camera.z_near) << 2;

	///////////////////////////////////////////////////////////////////
	// Render the triangle(s)
	// Various cases depending on which of the points are behind the camera
	switch(points_behind){
	case 7: // 111 - all points behind camera, do nothing
		return;
	case 0: { // 000 - all points in front of camera
		// Do perspective divide (into normalized device coordinates -> [-1, 1]
		tri_pos_clip.p1.xy /= (tri_pos_clip.p1.z);
		tri_pos_clip.p2.xy /= (tri_pos_clip.p2.z);
		tri_pos_clip.p3.xy /= (tri_pos_clip.p3.z);

		xen::Triangle3r tri_pos_screen =
			_convertToScreenSpaceTriOLD(tri_pos_clip, *cntx.viewport);

		_rasterizeTriangleScreen(cntx,
		                         tri_pos_world,  tri_nor_world,
		                         tri_pos_screen, tri_color, tri_uvs
		                        );
		return;
	}

	case 3:   // 011
		xen::swap(tri_pos_clip .p1, tri_pos_clip .p3);
		xen::swap(tri_pos_world.p1, tri_pos_world.p3);
		xen::swap(tri_nor_world.p1, tri_nor_world.p3);
		xen::swap(tri_color    .p1, tri_color    .p3);
		xen::swap(tri_uvs      .p1, tri_uvs      .p3);
		goto do_draw_2_behind;
	case 5:   // 101
		xen::swap(tri_pos_clip .p1, tri_pos_clip .p2);
		xen::swap(tri_pos_world.p1, tri_pos_world.p2);
		xen::swap(tri_nor_world.p1, tri_nor_world.p2);
		xen::swap(tri_color    .p1, tri_color    .p2);
		xen::swap(tri_uvs      .p1, tri_uvs      .p2);
		goto do_draw_2_behind;
	case 6:   // 110
	do_draw_2_behind: {
			// There are two points behind the camera, so just make a new triangle
			// between the single point in front of the camera, and where the lines
			// intersect z = 0 plane.
			//
			// Move point p2 and p3 along the line connecting them to p1 such that z
			// component becomes 0

			Vec4r delta_p2 = (tri_pos_clip.p1 - tri_pos_clip.p2);
			real  frac_p2  = (((-tri_pos_clip.p2.z + cntx.camera.z_near) / delta_p2.z));
			tri_pos_clip .p2  += (delta_p2                           ) * frac_p2;
			tri_pos_world.p2  += (tri_pos_world.p1 - tri_pos_world.p2) * frac_p2;
			tri_nor_world.p2  += (tri_nor_world.p1 - tri_nor_world.p2) * frac_p2;
			tri_color    .p2  += (tri_color    .p1 - tri_color    .p2) * frac_p2;
			tri_uvs      .p2  += (tri_uvs      .p1 - tri_uvs      .p2) * frac_p2;

			Vec4r delta_p3 = (tri_pos_clip.p1 - tri_pos_clip.p3);
			real  frac_p3  = (((-tri_pos_clip.p3.z + cntx.camera.z_near) / delta_p3.z));
			tri_pos_clip .p3  += (delta_p3                           ) * frac_p3;
			tri_pos_world.p3  += (tri_pos_world.p1 - tri_pos_world.p3) * frac_p3;
			tri_nor_world.p3  += (tri_nor_world.p1 - tri_nor_world.p3) * frac_p3;
			tri_color    .p3  += (tri_color    .p1 - tri_color    .p3) * frac_p3;
			tri_uvs      .p3  += (tri_uvs      .p1 - tri_uvs      .p3) * frac_p3;

			// Do perspective divide (into normalized device coordinates -> [-1, 1]
			tri_pos_clip.p1.xy /= (tri_pos_clip.p1.z);
			tri_pos_clip.p2.xy /= (tri_pos_clip.p2.z);
			tri_pos_clip.p3.xy /= (tri_pos_clip.p3.z);

			xen::Triangle3r tri_pos_screen =
				_convertToScreenSpaceTriOLD(tri_pos_clip, *cntx.viewport);

			_rasterizeTriangleScreen(cntx,
			                         tri_pos_world, tri_nor_world,
			                         tri_pos_screen, tri_color, tri_uvs);
			return;
		}
	case 1: // 001
		xen::swap(tri_pos_clip .p1, tri_pos_clip .p3);
		xen::swap(tri_pos_world.p1, tri_pos_world.p3);
		xen::swap(tri_nor_world.p1, tri_nor_world.p3);
		xen::swap(tri_color    .p1, tri_color    .p3);
		xen::swap(tri_uvs      .p1, tri_uvs      .p3);
		goto do_draw_1_behind;
	case 2: // 010
		xen::swap(tri_pos_clip .p2, tri_pos_clip .p3);
		xen::swap(tri_pos_world.p2, tri_pos_world.p3);
		xen::swap(tri_nor_world.p2, tri_nor_world.p3);
		xen::swap(tri_color    .p2, tri_color    .p3);
		xen::swap(tri_uvs      .p2, tri_uvs      .p3);
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

			Vec4r delta_p1       = (tri_pos_clip.p1 - tri_pos_clip.p3);
			real  frac_p1        = (((-tri_pos_clip.p3.z + cntx.camera.z_near) / delta_p1.z));
			Vec4r p3_slide_to_p1 = (tri_pos_clip.p3 + (delta_p1 * frac_p1));

			Vec4r delta_p2       = (tri_pos_clip.p2 - tri_pos_clip.p3);
			real  frac_p2        = (((-tri_pos_clip.p3.z + cntx.camera.z_near) / delta_p2.z));
			Vec4r p3_slide_to_p2 = (tri_pos_clip.p3 + (delta_p2 * frac_p2));

			xen::VertexGroup4r<4> quad_pos_clip;
			quad_pos_clip.vertices[0] = tri_pos_clip.p1;
			quad_pos_clip.vertices[1] = p3_slide_to_p1;
			quad_pos_clip.vertices[2] = tri_pos_clip.p2;
			quad_pos_clip.vertices[3] = p3_slide_to_p2;

			// perspective divide
			quad_pos_clip.vertices[0].xy /= quad_pos_clip.vertices[0].z;
			quad_pos_clip.vertices[1].xy /= quad_pos_clip.vertices[1].z;
			quad_pos_clip.vertices[2].xy /= quad_pos_clip.vertices[2].z;
			quad_pos_clip.vertices[3].xy /= quad_pos_clip.vertices[3].z;

			xen::VertexGroup3r<4> quad_pos_screen =
				_convertToScreenSpaceQuadOLD(quad_pos_clip, *cntx.viewport);

			xen::VertexGroup3r<4> quad_pos_world;
			quad_pos_world.vertices[0] = tri_pos_world.p1;
			quad_pos_world.vertices[1] = tri_pos_world.p3 + (tri_pos_world.p1 - tri_pos_world.p3) * frac_p1;
			quad_pos_world.vertices[2] = tri_pos_world.p2;
			quad_pos_world.vertices[3] = tri_pos_world.p3 + (tri_pos_world.p2 - tri_pos_world.p3) * frac_p2;

			xen::VertexGroup3r<4> quad_nor_world;
			quad_nor_world.vertices[0] = tri_nor_world.p1;
			quad_nor_world.vertices[1] = tri_nor_world.p3 + (tri_nor_world.p1 - tri_nor_world.p3) * frac_p1;
			quad_nor_world.vertices[2] = tri_nor_world.p2;
			quad_nor_world.vertices[3] = tri_nor_world.p3 + (tri_nor_world.p2 - tri_nor_world.p3) * frac_p2;

			xen::VertexGroup4f<4> quad_colors;
			quad_colors.vertices[0] = tri_color.p1;
			quad_colors.vertices[1] = tri_color.p3 + (tri_color.p1 - tri_color.p3) * frac_p1;
			quad_colors.vertices[2] = tri_color.p2;
			quad_colors.vertices[3] = tri_color.p3 + (tri_color.p2 - tri_color.p3) * frac_p2;

			xen::VertexGroup2f<4> quad_uvs;
			quad_uvs.vertices[0] = tri_uvs.p1;
			quad_uvs.vertices[1] = tri_uvs.p3 + (tri_uvs.p1 - tri_uvs.p3) * frac_p1;
			quad_uvs.vertices[2] = tri_uvs.p2;
			quad_uvs.vertices[3] = tri_uvs.p3 + (tri_uvs.p2 - tri_uvs.p3) * frac_p2;

			_rasterizeTriangleScreen(cntx,
			                         *(xen::Triangle3r*)&quad_pos_world .vertices[0],
			                         *(xen::Triangle3r*)&quad_nor_world .vertices[0],
			                         *(xen::Triangle3r*)&quad_pos_screen.vertices[0],
			                         *(xen::Triangle4f*)&quad_colors    .vertices[0],
			                         *(xen::Triangle2f*)&quad_uvs       .vertices[0]
			                        );
			_rasterizeTriangleScreen(cntx,
			                         *(xen::Triangle3r*)&quad_pos_world .vertices[1],
			                         *(xen::Triangle3r*)&quad_nor_world .vertices[1],
			                         *(xen::Triangle3r*)&quad_pos_screen.vertices[1],
			                         *(xen::Triangle4f*)&quad_colors    .vertices[1],
			                         *(xen::Triangle2f*)&quad_uvs       .vertices[1]
			                        );
		}
		return;
	}
} // end of function rasterizeTriangleModel


void rasterizeTrianglesModel(const RasterizationContext& cntx,
                             const Vec3r*                pos_model,
                             const Vec3r*                nor_model,
                             const Color*                col_buffer,
                             const Vec2f*                uvs_buffer,
                             const u32                   vertex_count){

	Triangle2f default_uvs    = { Vec2f::Origin, Vec2f::Origin, Vec2f::Origin };

	bool multiple_uvs = uvs_buffer != nullptr;
	if(uvs_buffer == nullptr){
		uvs_buffer = &default_uvs.vertices[0];
	}

	for(u32 i = 0; i < vertex_count - 1; i += 3){
		Triangle3r* tri_pos_model = (Triangle3r*)(&pos_model[i]);
		Triangle3r* tri_nor_model = (Triangle3r*)(&nor_model[i]);
		Triangle2f* tri_uvs = (Triangle2f*)(&uvs_buffer[i * multiple_uvs   ]);

		Triangle4f tri_col = { xen::Color::WHITE4f,
		                       xen::Color::WHITE4f,
		                       xen::Color::WHITE4f };
		if(col_buffer != nullptr){
			tri_col.vertices[0] = xen::makeColor4f(col_buffer[i+0]);
			tri_col.vertices[1] = xen::makeColor4f(col_buffer[i+1]);
			tri_col.vertices[2] = xen::makeColor4f(col_buffer[i+2]);
		}

		rasterizeTriangleModel(cntx, *tri_pos_model, *tri_nor_model, tri_col, *tri_uvs);
	}
} // end of rasterize triangles model


void rasterizeMesh(const RasterizationContext&    context,
                   xen::PrimitiveType             primitive_type,
                   const xen::MeshGeometrySource& mesh){

	switch(primitive_type){
	case xen::PrimitiveType::POINTS:
		rasterizePointsModel(context,
		                     mesh.position,
		                     mesh.color,
		                     mesh.vertex_count);
		break;
	case xen::PrimitiveType::LINES:
		rasterizeLinesModel(context,
		                    mesh.position,
		                    mesh.color,
		                    mesh.vertex_count,
		                    2); //advance by 2 vertices for each line drawn
		break;
	case xen::PrimitiveType::LINE_STRIP:
		rasterizeLinesModel(context,
		                    mesh.position,
		                    mesh.color,
		                    mesh.vertex_count,
		                    1); //advance by 1 vertex for each line drawn
		break;
	case xen::PrimitiveType::TRIANGLES: {
		rasterizeTrianglesModel(context,
		                        mesh.position,
		                        mesh.normal,
		                        mesh.color,
		                        mesh.uvs,
		                        mesh.vertex_count);
		break;
	}
	default:
		XenInvalidCodePath("Unhandled render command type in rasterizer device");
		break;
	}

} // end of rasterize mesh

} // end of namespace sren
} // end of namespace xen

#endif
