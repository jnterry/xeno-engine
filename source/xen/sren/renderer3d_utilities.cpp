////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of utility functions for the 3d renderer
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERER3D_UTILITIES_HPP
#define XEN_SREN_RENDERER3D_UTILITIES_HPP

#include <xen/graphics/Color.hpp>
#include <xen/math/geometry_types.hpp>
#include "RenderTargetImpl.hxx"

namespace xen {
	namespace sren {
		void clear(xen::sren::RenderTargetImpl& target, Color color) {
			Color4f color01 = (Color4f)color;
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.color[i] = color01;
			}
			for(u32 i = 0; i < target.width * target.height; ++i){
				target.depth[i] = FLT_MAX;
			}
		}

		void clear(RenderTargetImpl& target, const xen::Aabb2u& viewport, Color color){
			Color4f color01 = (Color4f)color;

			for(u32 y = viewport.min.y; y < viewport.max.y; ++y){
				// still need to stride by width of whole target to get to next line in
				// y, not just the width of the viewport
				u32 base = y * target.width;
				for(u32 x = viewport.min.x; x < viewport.max.x; ++x){
					target.color[base + x] = color01;
					target.depth[base + x] = FLT_MAX;
				}
			}
		}
	}
}

#endif
