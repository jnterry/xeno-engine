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
#include <xen/math/utilities.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/simd_intrinsics.hpp>

xsr::Framebuffer* xsr::createFramebuffer(xen::Allocator& alloc, Vec2u size){
	u32 pixel_count = size.x * size.y;
	xsr::Framebuffer* result  = (xsr::Framebuffer*)alloc.allocate
		( sizeof(xsr::Framebuffer) +
		  sizeof(xen::Color4f) * pixel_count +
		  sizeof(float)   * pixel_count +
		  alignof(float) + alignof(xen::Color4f)
		);

	if(result == nullptr){ return nullptr; }

	result->size  = size;
	result->color = (xen::Color4f*)xen::ptrGetAlignedForward(&result[1], alignof(xen::Color4f));
	result->depth = (float*  )xen::ptrGetAdvanced(result->color, sizeof(xen::Color4f) * pixel_count);
	xen::ptrAlignForward((void**)&result->depth, alignof(float));

	return result;
}

void xsr::destroyFramebuffer(xen::Allocator& alloc, xsr::Framebuffer* fb){
	alloc.deallocate(fb);
}

void xsr::putImageOnFramebuffer(xsr::Framebuffer* fb, const xen::RawImage& image){
	u32 max_x = xen::min(image.size.x, fb->size.x);
	u32 max_y = xen::min(image.size.y, fb->size.y);

	for(u32 y = 0; y < max_y; ++y){
		for(u32 x = 0; x < max_x; ++x){
			fb->color[y*fb->width + x].r = image.pixels[y*image.width + x].r / 255.0f;
			fb->color[y*fb->width + x].g = image.pixels[y*image.width + x].g / 255.0f;
			fb->color[y*fb->width + x].b = image.pixels[y*image.width + x].b / 255.0f;
			fb->color[y*fb->width + x].a = image.pixels[y*image.width + x].a / 255.0f;
		}
	}
}

#if XEN_USE_SSE
void xsr::getImageFromFramebuffer(const Framebuffer* fb, xen::RawImage& image){
	//////////////////////////////////////////////////////////////////////////
	// Set values of pixels - converting from our float colors to 32bit colors
	xen::Color4f color;
	u32     color_bits;
	float color_components_f[4];
	int*  color_components;

	// Generate 4 wide registers with single value broadcast to all components
	__m128  w_0    = _mm_set_ps1(  0.0f);
	__m128  w_1    = _mm_set_ps1(  1.0f);
	__m128  w_255  = _mm_set_ps1(255.0f);

	u32 max_x = xen::min(fb->width,  image.width);
	u32 max_y = xen::min(fb->height, image.height);

	for(u32 y = 0; y < max_y; ++y){

		u32 base_fb  = y * fb->width;
		u32 base_img = y * image.width;

		for(u32 x = 0; x < max_x; ++x){
			color = (fb->color[base_fb + x]);

			// load color components into wide register
			__m128 w_color = _mm_set_ps(color.a, color.r, color.g, color.b);

			// clamp to [0, 1] range
			w_color = _mm_min_ps(w_1, w_color); // clamp to be <= 1
			w_color = _mm_max_ps(w_0, w_color); // clamp to be >= 0

			// multiply up to [0, 255] range
			w_color = _mm_mul_ps(w_color, w_255);

			// Convert to 32 bit integers
			__m128i w_color_i = _mm_cvtps_epi32(w_color);

			// get integers out of wide register
			// sse2 has no instruction for storing ints, so we cheat...
			__m128  w_color_i_f = _mm_castsi128_ps(w_color_i); // cast ints to floats
			_mm_store_ps(color_components_f, w_color_i_f);     // store floats in memory
			color_components = (int*)color_components_f;       // cast back to ints

			// Set the pixel value
			color_bits = (color_components[3] << 24 |
			              color_components[2] << 16 |
			              color_components[1] <<  8 |
			              color_components[0] <<  0
			              );

			image.pixels[base_img + x].value = color_bits;
		}
	}
}
#else
void xsr::getImageFromFramebuffer(const xsr::Framebuffer* fb, xen::RawImage& image){
	//////////////////////////////////////////////////////////////////////////
	// Set values of pixels - converting from our float colors to 32bit colors
	xen::Color4f color;
	u32     color_bits;

	u32 max_x = xen::min(fb->width,  image.width);
	u32 max_y = xen::min(fb->height, image.height);

	for(u32 y = 0; y < max_y; ++y){
		for(u32 x = 0; x < max_x; ++x){
			color = (fb->color[y * fb->width + x]);

			color_bits = (xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.a) << 24 |
			              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.r) << 16 |
			              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.g) <<  8 |
			              xen::mapToRangeClamped<float, u32>(0.0f, 1.0f, 0, 255, color.b) <<  0);

			image.pixels[y * image.width + x].value = color_bits;
		}
	}
}
#endif

#endif
