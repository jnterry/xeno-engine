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
#include <xen/graphics/Image.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/utilities.hpp>

namespace xen {
	namespace sren {
		Framebuffer* createFramebuffer(xen::Allocator& alloc, Vec2u size){
			u32 pixel_count = size.x * size.y;
			Framebuffer* result  = (Framebuffer*)alloc.allocate(sizeof(Framebuffer) +
			                                                    sizeof(Color4f) * pixel_count +
			                                                    sizeof(float)   * pixel_count +
			                                                    alignof(float) + alignof(Color4f)
			                                                    );

			if(result == nullptr){ return nullptr; }

			result->color = (Color4f*)xen::ptrGetAlignedForward(&result[1], alignof(Color4f));
			result->depth = (float*  )xen::ptrGetAdvanced(result->color, sizeof(Color4f) * pixel_count);
			xen::ptrAlignForward((void**)&result->depth, alignof(float));

			return result;
		}

		void destroyFramebuffer(xen::Allocator& alloc, Framebuffer* fb){
			alloc.deallocate(fb);
		}

		void putImageOnFramebuffer(Framebuffer* fb, const RawImage& image, float depth_val){
			printf("Put image on frame buffer NOT IMPLEMENTED\n");
		}

		void getImageFromFramebuffer(const Framebuffer* fb, RawImage& image){
			printf("Get image from frame buffer NOT IMPLEMENTED\n");

			for(u32 y = 0; y < image.size.y; ++y){
				for(u32 x = 0; x < image.size.x; ++x){
					printf("put pixel %i, %i\n", x, y);
					image.pixels[y*image.width + x].r = x % 255;
					image.pixels[y*image.width + x].g = y % 255;
					image.pixels[y*image.width + x].b = 100;
					image.pixels[y*image.width + x].a = 255;
				}
			}
		}
	}
}

#endif
