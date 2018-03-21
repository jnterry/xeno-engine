////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation for the invert colors post processor
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_POSTPROCESSORS_DISPLAYDEPTHBUFFER_CPP
#define XEN_SREN_POSTPROCESSORS_DISPLAYDEPTHBUFFER_CPP

#include <xen/sren/PostProcessor.hpp>
#include <xen/math/utilities.hpp>

#include <cfloat>
#include <cmath>

namespace xen {
	namespace sren {

		void PostProcessorDisplayDepthBuffer::process(FrameBuffer& fb) {

			Aabb2u viewport;
			viewport.min = (Vec2u)((Vec2r)fb.size * this->screen_region.min);
			viewport.max = (Vec2u)((Vec2r)fb.size * this->screen_region.max);

			for(u32 y = viewport.min.y; y < viewport.max.y; ++y){
				u32 pixel_index_base = y * fb.width;
				for(u32 x = viewport.min.x; x < viewport.max.x; ++x){
					u32 pixel_index = pixel_index_base + x;

					Vec2u depth_location;
					depth_location.x = xen::mapToRange<u32, u32>(viewport.min.x, viewport.max.x,
					                                             0, fb.size.x,
					                                             x);
					depth_location.y = xen::mapToRange<u32, u32>(viewport.min.y, viewport.max.y,
					                                             0, fb.size.y,
					                                             y);

					float depth_val = fb.depth[depth_location.y * fb.width + depth_location.x];
					if(depth_val == FLT_MAX){
						depth_val = 0;
					} else {
						depth_val = xen::mapToRange<real, float>(this->z_near, this->z_far,
						                                         1.0f, 0.0f,
						                                         depth_val);

						// :TODO: this is to make changes in depth near the camera more
						//  pronounced in color space than those far from camera. IE:
						// difference in color between depth 1 and 2 is much greater color
						// change than between depth 100 and 101.
						// can we do something better -> at very least make this pow
						// configurable
						depth_val = powf(depth_val, 200.0f);
					}


					fb.color[pixel_index].r = depth_val;
					fb.color[pixel_index].g = depth_val;
					fb.color[pixel_index].b = depth_val;
					fb.color[pixel_index].a = 1.0f;
				}
			}
		}

	}
}

#endif
