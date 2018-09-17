////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for antialiasing as a post-processor step
/// inspired by FXAA, with some approximations to improve performance
/// Based on: http://blog.simonrodriguez.fr/articles/30-07-2016_implementing_fxaa.html
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////


#ifndef XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP
#define XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP

#include <xen/sren/PostProcessor.hpp>
#include <xen/math/utilities.hpp>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <cstring>

// Recommended values for constants to determine presence of edge
#define EDGE_THRESHOLD_MIN 0.0312
#define EDGE_THRESHOLD_MAX 0.125
// Maximum length of edge in each direction
#define ITERATIONS         12

namespace {
	/// \brief get luma of a given color value
	real rgb2luma(Vec3f rgb){
		return sqrt(dot(rgb, Vec3f(0.299, 0.587, 0.114)));
	}

	/// \brief get color of a position between pixels by bilinear filtering 4 nearest pixels
	Vec3f blendColor(xsr::Framebuffer& fb, Vec2r subPixelLocation){
		int topLeftIndex     = floor(subPixelLocation.y)*fb.size.x + floor(subPixelLocation.x);
		int bottomLeftIndex  = ceil(subPixelLocation.y)*fb.size.x  + floor(subPixelLocation.x);
		int topRightIndex    = floor(subPixelLocation.y)*fb.size.x + ceil(subPixelLocation.x);
		int bottomRightIndex = ceil(subPixelLocation.y)*fb.size.x  + ceil(subPixelLocation.x);

		return (0.25 * fb.color[topLeftIndex].rgb + 0.25 * fb.color[bottomLeftIndex].rgb + 0.25 * fb.color[topRightIndex].rgb + 0.25 * fb.color[bottomRightIndex].rgb);
	}

	void fxaaStep(xsr::Framebuffer& fb, xsr::Framebuffer& fb_out,
	              int x, int y,
	              Vec2r inverseScreenSize
	             ) {
		int currentPixelIndex = y*fb.size.x + x;

		// Luma at the current fragment
		real lumaCenter = rgb2luma(fb.color[currentPixelIndex].rgb);

		// Luma at the four direct neighbours of the current fragment.
		real lumaDown   = rgb2luma(fb.color[(y+1)*fb.size.x + (x  )].rgb);
		real lumaUp     = rgb2luma(fb.color[(y-1)*fb.size.x + (x  )].rgb);
		real lumaLeft   = rgb2luma(fb.color[(y  )*fb.size.x + (x-1)].rgb);
		real lumaRight  = rgb2luma(fb.color[(y  )*fb.size.x + (x+1)].rgb);

		// Find the maximum and minimum luma around the current fragment.
		real lumaMin = xen::min(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
		real lumaMax = xen::max(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
		// Compute the delta.
		real lumaRange = lumaMax - lumaMin;

		// If the luma variation is lower that a threshold (or if we are in a really dark area),
		// we are not on an edge, don't perform any AA.
		if(lumaRange < xen::max(EDGE_THRESHOLD_MIN,lumaMax*EDGE_THRESHOLD_MAX)){
			return;
		}

		// Query the 4 remaining corners lumas.
		real lumaDownLeft  = rgb2luma(fb.color[(y+1)*fb.size.x + (x-1)].rgb);
		real lumaUpRight   = rgb2luma(fb.color[(y-1)*fb.size.x + (x+1)].rgb);
		real lumaUpLeft    = rgb2luma(fb.color[(y-1)*fb.size.x + (x-1)].rgb);
		real lumaDownRight = rgb2luma(fb.color[(y+1)*fb.size.x + (x+1)].rgb);

		// Combine the four edges lumas (using intermediary variables for future computations with the same values).
		real lumaDownUp = lumaDown + lumaUp;
		real lumaLeftRight = lumaLeft + lumaRight;
		// Same for corners
		real lumaLeftCorners = lumaDownLeft + lumaUpLeft;
		real lumaDownCorners = lumaDownLeft + lumaDownRight;
		real lumaRightCorners = lumaDownRight + lumaUpRight;
		real lumaUpCorners = lumaUpRight + lumaUpLeft;

		// Compute an estimation of the gradient along the horizontal and vertical axis.
		real edgeHorizontal = std::fabs(-2.0 * lumaLeft + lumaLeftCorners)
			+ std::fabs(-2.0 * lumaCenter + lumaDownUp ) * 2.0
			+ std::fabs(-2.0 * lumaRight + lumaRightCorners);
		real edgeVertical   = std::fabs(-2.0 * lumaUp + lumaUpCorners)
			+ std::fabs(-2.0 * lumaCenter + lumaLeftRight) * 2.0
			+ std::fabs(-2.0 * lumaDown + lumaDownCorners);

		// Is the local edge horizontal or vertical ?
		bool isHorizontal = (edgeHorizontal >= edgeVertical);

		// Select the two neighboring pixel lumas in the opposite direction to the local edge.
		real luma1 = isHorizontal ? lumaDown : lumaLeft;
		real luma2 = isHorizontal ? lumaUp : lumaRight;
		// Compute gradients in this direction.
		real gradient1 = luma1 - lumaCenter;
		real gradient2 = luma2 - lumaCenter;

		// Which direction is the steepest ?
		bool is1Steepest = std::fabs(gradient1) >= std::fabs(gradient2);

		// Gradient in the corresponding direction, normalized.
		real gradientScaled = 0.25*xen::max(std::fabs(gradient1),std::fabs(gradient2));
		// Choose the step size (one pixel) according to the edge direction.
		real stepLength = isHorizontal ? inverseScreenSize.y : inverseScreenSize.x;

		// Average luma in the correct direction.
		real lumaLocalAverage = 0.0;

		if(is1Steepest){
			// Switch the direction
			stepLength = - stepLength;
			lumaLocalAverage = 0.5*(luma1 + lumaCenter);
		} else {
			lumaLocalAverage = 0.5*(luma2 + lumaCenter);
		}

		// Shift UV in the correct direction by half a pixel.
		Vec2r currentUv = Vec2r{(real)x,(real)y};
		if(isHorizontal){
			currentUv.y += stepLength * 0.5;
		} else {
			currentUv.x += stepLength * 0.5;
		}

		// Compute offset (for each iteration step) in the right direction.
		Vec2r offset = isHorizontal ? Vec2r{inverseScreenSize.x, 0.0} : Vec2r{0.0,inverseScreenSize.y};
		// Compute UVs to explore on each side of the edge, orthogonally. The QUALITY allows us to step faster.
		Vec2r uv1 = currentUv - offset;
		Vec2r uv2 = currentUv + offset;

		// Read the lumas at both current extremities of the exploration segment, and compute the delta wrt to the local average luma.
		real lumaEnd1 = rgb2luma(blendColor(fb, uv1));
		real lumaEnd2 = rgb2luma(blendColor(fb, uv2));
		lumaEnd1 -= lumaLocalAverage;
		lumaEnd2 -= lumaLocalAverage;

		// If the luma deltas at the current extremities are larger than the local gradient, we have reached the side of the edge.
		bool reached1 = fabs(lumaEnd1) >= gradientScaled;
		bool reached2 = fabs(lumaEnd2) >= gradientScaled;
		bool reachedBoth = reached1 && reached2;

		// If the side is not reached, we continue to explore in this direction.
		if(!reached1){
			uv1 -= offset;
		}
		if(!reached2){
			uv2 += offset;
		}

		// If both sides have not been reached, continue to explore.
		if(!reachedBoth){

			for(int i = 2; i < ITERATIONS; i++){
				// If needed, read luma in 1st direction, compute delta.
				if(!reached1){
					lumaEnd1 = rgb2luma(blendColor(fb, uv1));
					lumaEnd1 = lumaEnd1 - lumaLocalAverage;
				}
				// If needed, read luma in opposite direction, compute delta.
				if(!reached2){
					lumaEnd2 = rgb2luma(blendColor(fb, uv2));
					lumaEnd2 = lumaEnd2 - lumaLocalAverage;
				}
				// If the luma deltas at the current extremities is larger than the local gradient, we have eached he side of the edge.
				reached1 = fabs(lumaEnd1) >= gradientScaled;
				reached2 = fabs(lumaEnd2) >= gradientScaled;
				reachedBoth = reached1 && reached2;

				// If the side is not reached, we continue to explore in this direction
				if(!reached1){
					uv1 -= offset;
				}
				if(!reached2){
					uv2 += offset;
				}

				// If both sides have been reached, stop the exploration.
				if(reachedBoth){ break;}
			}
		}

		// Compute the distances to each extremity of the edge.
		real distance1 = isHorizontal ? (x - uv1.x) : (y - uv1.y);
		real distance2 = isHorizontal ? (uv2.x - x) : (uv2.y - y);

		// In which direction is the extremity of the edge closer ?
		bool isDirection1 = distance1 < distance2;
		real distanceFinal = xen::min(distance1, distance2);

		// Length of the edge.
		real edgeThickness = (distance1 + distance2);

		// UV offset: read in the direction of the closest side of the edge.
		real pixelOffset = - distanceFinal / edgeThickness + 0.5;

		// Is the luma at center smaller than the local average ?
		bool isLumaCenterSmaller = lumaCenter < lumaLocalAverage;

		// If the luma at center is smaller than at its neighbour, the delta luma at each end should be positive (same variation).
		// (in the direction of the closer side of the edge.)
		bool correctVariation = ((isDirection1 ? lumaEnd1 : lumaEnd2) < 0.0) != isLumaCenterSmaller;

		// If the luma variation is incorrect, do not offset.
		real finalOffset = correctVariation ? pixelOffset : 0.0;

		// Compute the final UV coordinates.
		Vec2r finalUv = Vec2r{(real)x,(real)y};
		if(isHorizontal){
			finalUv.y += finalOffset * stepLength;
		} else {
			finalUv.x += finalOffset * stepLength;
		}

		// Read the color at the new UV coordinates, and use it.
		Vec3f finalColor = blendColor(fb, finalUv);

		fb_out.color[currentPixelIndex].r = finalColor.r;
		fb_out.color[currentPixelIndex].g = finalColor.g;
		fb_out.color[currentPixelIndex].b = finalColor.b;
	}
} // end of anon namespace

void xsr::PostProcessorAntialias::process(xsr::Framebuffer& fb) {
	Vec2r inverseScreenSize = {1.0, 1.0};

	// Create a duplicate frame buffer
	xsr::Framebuffer fb_copy;
	fb_copy.width  = fb.width;
	fb_copy.height = fb.height;

	// :TODO: -> don't really want to be allocating every frame
	fb_copy.color = (xen::Color4f*)malloc(fb_copy.height*fb_copy.width*sizeof(xen::Color4f));

	memcpy(fb_copy.color, fb.color, (fb_copy.height*fb_copy.width*sizeof(xen::Color4f)));

	for(u32 j = 1; j < fb.size.y-1; ++j){
		for(u32 i = 1; i < fb.size.x-1; ++i){
			fxaaStep(fb_copy, fb, i, j, inverseScreenSize);
		}
	}

	free(fb_copy.color);
}

#endif
