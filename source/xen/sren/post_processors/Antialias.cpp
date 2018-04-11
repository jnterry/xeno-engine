////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for antialiasing as a post-processor step, such as FXAA
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

// GPU based example of FXAA: http://blog.simonrodriguez.fr/articles/30-07-2016_implementing_fxaa.html
// FXAA Whitepaper: http://developer.download.nvidia.com/assets/gamedev/files/sdk/11/FXAA_WhitePaper.pdf

// :TODO: What to do if Any luma are out of bounds, such as pixels on edges?
// :TODO: Check if top left or bottom left is fb[0], if bottom left then swap lumaUp and lumaDown
// :TODO: Define QUALITY(i) -> currently don't know how

#ifndef XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP
#define XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP

#include <xen/sren/PostProcessor.hpp>
#include <xen/math/utilities.hpp>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Recommended values for constants to determine presence of edge
#define EDGE_THRESHOLD_MIN 0.0312
#define EDGE_THRESHOLD_MAX 0.125
// Maximum length of edge in each direction
#define ITERATIONS         12
// Value for antialiasing subpixel lines
#define SUBPIXEL_QUALITY 0.75

namespace xen {
	namespace sren {
		/// brief: get luma of a given color value
		real rgb2luma(Vec3f rgb){
		    return sqrt(dot(rgb, Vec3f(0.299, 0.587, 0.114)));
		}

		/// brief: get color of a position between pixels by bilinear filtering 4 nearest pixels
		// :TODO: May want to just pass in specific pixels to this function
		Vec3f blendColor(Framebuffer& fb, Vec2r subPixelLocation){
			real ratioVertical   = (subPixelLocation.x) - floor(subPixelLocation.x);
		  real ratioHorizontal = (subPixelLocation.y) - floor(subPixelLocation.y);

			// find color average of left pair of pixels
			int topLeftIndex    = floor(subPixelLocation.y)*fb.size.x + floor(subPixelLocation.x);
			int bottomLeftIndex = ceil(subPixelLocation.y)*fb.size.x  + floor(subPixelLocation.x);
			Vec3f colorLeft = lerp(fb.color[topLeftIndex].rgb, fb.color[bottomLeftIndex].rgb, ratioVertical);

			// find color average of right pair of pixels
			int topRightIndex    = floor(subPixelLocation.y)*fb.size.x + ceil(subPixelLocation.x);
			int bottomRightIndex = ceil(subPixelLocation.y)*fb.size.x  + ceil(subPixelLocation.x);
			Vec3f colorRight = lerp(fb.color[topRightIndex].rgb, fb.color[bottomRightIndex].rgb, ratioVertical);

			return lerp(colorLeft, colorRight, ratioHorizontal);
		}

		real clip(real n, real lower, real upper) {
		  return max(lower, min(n, upper));
		}

		void fxaaStep(Framebuffer& fb, int x, int y, Vec2r inverseScreenSize) {
			int currentPixelIndex = y*fb.size.x + x;

			// Luma at the current fragment
			real lumaCenter = rgb2luma(fb.color[currentPixelIndex].rgb);

			// :TODO: Determine what to do when pixels are on edge of the image
			// Luma at the four direct neighbours of the current fragment.
			real lumaDown   = rgb2luma(fb.color[(y+1)*fb.size.x + (x  )].rgb);
			real lumaUp     = rgb2luma(fb.color[(y-1)*fb.size.x + (x  )].rgb);
			real lumaLeft   = rgb2luma(fb.color[(y  )*fb.size.x + (x-1)].rgb);
			real lumaRight  = rgb2luma(fb.color[(y  )*fb.size.x + (x+1)].rgb);

			// Find the maximum and minimum luma around the current fragment.
			real lumaMin = min(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
			real lumaMax = max(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
			// Compute the delta.
			real lumaRange = lumaMax - lumaMin;

			// If the luma variation is lower that a threshold (or if we are in a really dark area),
			// we are not on an edge, don't perform any AA.
			if(lumaRange < max(EDGE_THRESHOLD_MIN,lumaMax*EDGE_THRESHOLD_MAX)){
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
			real edgeHorizontal = std::abs(-2.0 * lumaLeft + lumaLeftCorners)
			                    + std::abs(-2.0 * lumaCenter + lumaDownUp ) * 2.0
													+ std::abs(-2.0 * lumaRight + lumaRightCorners);
			real edgeVertical   = std::abs(-2.0 * lumaUp + lumaUpCorners)
			                    + std::abs(-2.0 * lumaCenter + lumaLeftRight) * 2.0
													+ std::abs(-2.0 * lumaDown + lumaDownCorners);

			// Is the local edge horizontal or vertical ?
			bool isHorizontal = (edgeHorizontal >= edgeVertical);

			// Select the two neighboring pixel lumas in the opposite direction to the local edge.
			real luma1 = isHorizontal ? lumaDown : lumaLeft;
			real luma2 = isHorizontal ? lumaUp : lumaRight;
			// Compute gradients in this direction.
			real gradient1 = luma1 - lumaCenter;
			real gradient2 = luma2 - lumaCenter;

			// Which direction is the steepest ?
			bool is1Steepest = std::abs(gradient1) >= std::abs(gradient2);

			// Gradient in the corresponding direction, normalized.
			real gradientScaled = 0.25*max(std::abs(gradient1),std::abs(gradient2));
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
			Vec2r currentUv = Vec2r{x,y};
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
			bool reached1 = abs(lumaEnd1) >= gradientScaled;
			bool reached2 = abs(lumaEnd2) >= gradientScaled;
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
			    reached1 = abs(lumaEnd1) >= gradientScaled;
			    reached2 = abs(lumaEnd2) >= gradientScaled;
			    reachedBoth = reached1 && reached2;

			    // If the side is not reached, we continue to explore in this direction, with a variable quality.
			    if(!reached1){
			      uv1 -= offset;// * QUALITY(i);
			    }
			    if(!reached2){
			      uv2 += offset;// * QUALITY(i);
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
			real distanceFinal = min(distance1, distance2);

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

			// Sub-pixel shifting
			// Full weighted average of the luma over the 3x3 neighborhood.
			real lumaAverage = (1.0/12.0) * (2.0 * (lumaDownUp + lumaLeftRight) + lumaLeftCorners + lumaRightCorners);
			// Ratio of the delta between the global average and the center luma, over the luma range in the 3x3 neighborhood.
			real subPixelOffset1 = clip(abs(lumaAverage - lumaCenter)/lumaRange,0.0,1.0);
			real subPixelOffset2 = (-2.0 * subPixelOffset1 + 3.0) * subPixelOffset1 * subPixelOffset1;
			// Compute a sub-pixel offset based on this delta.
			real subPixelOffsetFinal = subPixelOffset2 * subPixelOffset2 * SUBPIXEL_QUALITY;

			// Pick the biggest of the two offsets.
			finalOffset = max(finalOffset,subPixelOffsetFinal);

			// Compute the final UV coordinates.
			Vec2r finalUv = Vec2r{x,y};
			if(isHorizontal){
			    finalUv.y += finalOffset * stepLength;
			} else {
			    finalUv.x += finalOffset * stepLength;
			}

			// Read the color at the new UV coordinates, and use it.
			Vec3f finalColor = blendColor(fb, finalUv);

			fb.color[currentPixelIndex].r = finalColor.r;
			fb.color[currentPixelIndex].g = finalColor.g;
			fb.color[currentPixelIndex].b = finalColor.b;
		}

		void PostProcessorAntialias::process(Framebuffer& fb) {
			Vec2r inverseScreenSize = {1.0/fb.size.x, 1.0/fb.size.y};
			// :TODO: Fix these loops to be between 0 and size; changed to this for test purposes
			for(u32 j = 1; j < fb.size.y-1; ++j){
				for(u32 i = 1; i < fb.size.x-1; ++i){
					fxaaStep(fb, i, j, inverseScreenSize);
				}
			}
		}

	}
}

#endif
