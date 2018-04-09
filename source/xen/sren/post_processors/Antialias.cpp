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

// Recommended values for constants to determine presence of edge
#define EDGE_THRESHOLD_MIN 0.0312
#define EDGE_THRESHOLD_MAX 0.125

namespace xen {
	namespace sren {

		float rgb2luma(Vec3f rgb){
		    return sqrt(dot(rgb, Vec3f(0.299, 0.587, 0.114)));
		}

		void fxaaStep(Framebuffer& fb, int x, int y, Vec2r inverseScreenSize) {
			int currentPixel = y*fb.size.x + x;

			// Luma at the current fragment
			float lumaCenter = rgb2luma(fb.color[currentPixel].rgb);
			// :TODO: What to do if these are out of bounds?
			// Luma at the four direct neighbours of the current fragment.
			float lumaDown   = rgb2luma(fb.color[(y+1)*fb.size.x + (x  )].rgb);
			float lumaUp     = rgb2luma(fb.color[(y-1)*fb.size.x + (x  )].rgb);
			float lumaLeft   = rgb2luma(fb.color[(y  )*fb.size.x + (x-1)].rgb);
			float lumaRight  = rgb2luma(fb.color[(y  )*fb.size.x + (x+1)].rgb);

			// Find the maximum and minimum luma around the current fragment.
			float lumaMin = min(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
			float lumaMax = max(lumaCenter, lumaDown, lumaUp, lumaLeft, lumaRight);
			// Compute the delta.
			float lumaRange = lumaMax - lumaMin;

			// If the luma variation is lower that a threshold (or if we are in a really dark area),
			// we are not on an edge, don't perform any AA.
			if(lumaRange < max(EDGE_THRESHOLD_MIN,lumaMax*EDGE_THRESHOLD_MAX)){
    		return;
			}
			// :TODO: Complete FXAA tutorial, as above
			//        Finsished as far as "Estimating gradient and choosing edge direction"
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
