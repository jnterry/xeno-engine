////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for postProcessor which, for a given
/// colour, adds fog to scene as depth increases
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////


#ifndef XEN_SREN_POSTPROCESSORS_DEPTHFOG_CPP
#define XEN_SREN_POSTPROCESSORS_DEPTHFOG_CPP

#include <xen/sren/PostProcessor.hpp>

void xsr::PostProcessorDepthFog::process(xsr::Framebuffer& fb) {
	real z_diff = z_far - z_near;

	for(u32 i = 0; i < fb.size.x * fb.size.y; ++i){

		if (fb.depth[i] <= z_near){
			continue;
		}
		if (fb.depth[i] >= z_far){
			fb.color[i].rgb = (fb.color[i].rgb * (1-fog_color.a) + (fog_color.rgb * fog_color.a));
			continue;
		}
		real z_ratio = ((fb.depth[i] - z_near) / z_diff)*fog_color.a;
		fb.color[i].rgb = (fb.color[i].rgb * (1-z_ratio) + (fog_color.rgb * z_ratio));
	}
}

#endif
