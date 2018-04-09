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

namespace xen {
	namespace sren {

		float rgb2luma(Vec3f rgb){
		    return sqrt(dot(rgb, Vec3f(0.299, 0.587, 0.114)));
		}

		void fxaaStep(Framebuffer& fb, int x, int y, Vec2r inverseScreenSize) {
			// Luma at the current fragment
			int currentPixel = y*fb.size.x + x;

			float lumaCenter = rgb2luma(fb.color[currentPixel].rgb);
		}

		void PostProcessorAntialias::process(Framebuffer& fb) {
			Vec2r inverseScreenSize = {1.0/fb.size.x, 1.0/fb.size.y};

			for(u32 j = 0; j < fb.size.y; ++j){
				for(u32 i = 0; i < fb.size.x; ++i){
					int currentPixel = j*fb.size.x + i;

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
