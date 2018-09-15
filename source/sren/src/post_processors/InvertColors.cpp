////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for the invert colors post processor
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_POSTPROCESSORS_INVERTCOLORS_CPP
#define XEN_SREN_POSTPROCESSORS_INVERTCOLORS_CPP

#include <xen/sren/PostProcessor.hpp>

void xsren::PostProcessorInvertColors::process(xsren::Framebuffer& fb) {
	for(u32 i = 0; i < fb.size.x * fb.size.y; ++i){
		fb.color[i].r = 1.0f - fb.color[i].r;
		fb.color[i].g = 1.0f - fb.color[i].g;
		fb.color[i].b = 1.0f - fb.color[i].b;
	}
}

#endif
