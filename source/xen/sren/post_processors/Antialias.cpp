////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for antialiasing as a post-processor step, such as FXAA
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

// GPU based example of FXAA: http://blog.simonrodriguez.fr/articles/30-07-2016_implementing_fxaa.html
// FXAA Whitepaper: http://developer.download.nvidia.com/assets/gamedev/files/sdk/11/FXAA_WhitePaper.pdf

#ifndef XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP
#define XEN_SREN_POSTPROCESSORS_ANTIALIAS_CPP

#include <xen/sren/PostProcessor.hpp>
#include <stdlib.h>

// Recommended values for constants to determine presence of edge
#define EDGE_THRESHOLD_MIN 0.0312
#define EDGE_THRESHOLD_MAX 0.125

namespace xen {
	namespace sren {

		real rgb2luma(Vec3f rgb){
		    return sqrt(dot(rgb, Vec3f(0.299, 0.587, 0.114)));
		}

		void fxaaStep(Framebuffer& fb, int x, int y, Vec2r inverseScreenSize) {
			int currentPixel = y*fb.size.x + x;

			// Luma at the current fragment
			real lumaCenter = rgb2luma(fb.color[currentPixel].rgb);
			// :TODO: What to do if these are out of bounds?
			// :TODO: Check if top left or bottom left is fb[0], if bottom left then swap up and down
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

			// :TODO: How to translate this into xen code?
			// Shift UV in the correct direction by half a pixel.
			Vec2r currentUv = In.uv;
			if(isHorizontal){
    		currentUv.y += stepLength * 0.5;
			} else {
    		currentUv.x += stepLength * 0.5;
			}

			// :TODO: Continue from 'First iteration exploration' in tutorial
		}

		void PostProcessorAntialias::process(Framebuffer& fb) {
			Vec2r inverseScreenSize = {1.0/fb.size.x, 1.0/fb.size.y};

			for(u32 j = 0; j < fb.size.y; ++j){
				for(u32 i = 0; i < fb.size.x; ++i){
					int currentPixel = j*fb.size.x + i;

					// :TODO: Remove these
			  	fb.color[currentPixel].r = 1.0f - fb.color[currentPixel].r;
			  	fb.color[currentPixel].g = 1.0f - fb.color[currentPixel].g;
			  	fb.color[currentPixel].b = 1.0f - fb.color[currentPixel].b;

					fxaaStep(fb, i, j, inverseScreenSize);
				}
			}
		}

	}
}

#endif
