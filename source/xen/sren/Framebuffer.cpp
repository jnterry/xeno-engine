////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of Framebuffer related functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_FRAMEBUFFER_CPP
#define XEN_SREN_FRAMEBUFFER_CPP

#include <xen/sren/Framebuffer.hpp>

namespace xen {
	namespace sren {
		Framebuffer* createFramebuffer(xen::Allocator& alloc, Vec2u size){
			printf("Creating framebuffer NOT IMPLEMENTED\n");
			return nullptr;
		}

		void destroyFramebuffer(xen::Allocator& alloc, Framebuffer* fb){
			printf("Destroying frame buffer NOT IMPLEMENTED\n");
		}

		void putImageOnFramebuffer(Framebuffer* fb, const RawImage& image, float depth_val){
			printf("Put image on frame buffer NOT IMPLEMENTED\n");
		}

		void getImageFromFramebuffer(const Framebuffer* fb, RawImage& image){
			printf("Get image from frame buffer NOT IMPLEMENTED\n");
		}
	}
}

#endif
