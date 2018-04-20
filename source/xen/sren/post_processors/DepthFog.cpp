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
#include <cstdio>

namespace xen {
	namespace sren {

		void PostProcessorDepthFog::process(Framebuffer& fb) {
			real z_diff = z_far - z_near;

			for(u32 i = 0; i < fb.size.x * fb.size.y; ++i){

				if (fb.depth[i] <= z_near){
					continue;
				}
				if (fb.depth[i] >= z_far){
					fb.color[i] = fog_color;
					continue;
				}
				real z_ratio = (fb.depth[i] - z_near) / z_diff;
				fb.color[i] = (fb.color[i] * (1-z_ratio) + (fog_color * z_ratio));
			}
		}

	}
}

#endif
