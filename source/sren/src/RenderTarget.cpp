////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Includes platform specific implementation for sren::RenderTargetImpl
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_CPP
#define XEN_SREN_RENDERTARGETIMPL_CPP

#include <xen/sren/RenderTarget.hxx>
#include <xen/graphics/Color.hpp>

void xsr::clear(xsr::RenderTarget& target, xen::Color color) {
	xen::Color4f color01 = (xen::Color4f)color;
	for(u32 i = 0; i < target.width * target.height; ++i){
		target.color[i] = color01;
	}
	for(u32 i = 0; i < target.width * target.height; ++i){
		target.depth[i] = FLT_MAX;
	}
}

void xsr::clear(xsr::RenderTarget& target, const xen::Aabb2u& viewport, xen::Color color){
	xen::Color4f color01 = (xen::Color4f)color;

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

#include <xen/config.hpp>
#ifdef XEN_OS_UNIX
	#include "RenderTarget.unix.cpp"
#elif defined XEN_OS_WINDOWS
	#include "RenderTarget.win.cpp"
#else
	#error "Software renderer not supported on this platform!"
#endif

#endif
