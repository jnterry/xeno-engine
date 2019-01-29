////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Windows specific implementation of software render target
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_WIN_CPP
#define XEN_SREN_RENDERTARGETIMPL_WIN_CPP

#include <xen/core/memory/Allocator.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/window/Window.hxx>
#include <xen/sren/RenderTarget.hxx>

void xsr::doPlatformRenderTargetInit(xen::Allocator* alloc,
                                       xsr::RenderTarget& target,
                                       xen::Window* window){
	if(window == nullptr || window->handle == nullptr){
		// offscreen render targets don't need windows bitmapinfo, etc
		return;
	}

	u32 num_pixels = target.width * target.height;

	// ensure allocated array is multiply of 4 so we can operate on it with 4
	//wide simd instructions with no special cases
	num_pixels += num_pixels % 4;

	target.pixels = (xen::Color*)alloc->allocate(sizeof(xen::Color) * num_pixels);

	// :TODO: some aspects of this should depend on the window's pixel format (eg, bit count)
	target.bitmap_info = {0};
	target.bitmap_info.bmiHeader.biSize        = sizeof(BITMAPINFOHEADER);
	target.bitmap_info.bmiHeader.biWidth       = (LONG)target.width;
	target.bitmap_info.bmiHeader.biHeight      = (LONG)target.height;
	target.bitmap_info.bmiHeader.biPlanes      = 1;
	target.bitmap_info.bmiHeader.biBitCount    = 32;
	target.bitmap_info.bmiHeader.biCompression = BI_RGB;

	// :TODO: log
	printf("Created software render target for window\n");
}

void xsr::doPlatformRenderTargetDestruction(xen::Allocator* alloc,
                                              xsr::RenderTarget& target,
                                              xen::Window* window){
	alloc->deallocate(target.pixels);
}

void xsr::doPlatformRenderTargetResize(xen::Allocator* alloc,
                                         xsr::RenderTarget& target,
                                         xen::Window* window) {
	doPlatformRenderTargetDestruction(alloc, target, window);
	doPlatformRenderTargetInit(alloc, target, window);
}

void xsr::presentRenderTarget(xen::Window* window, xsr::RenderTarget& target){
	//////////////////////////////////////////////////////////////////////////
	// Update the byte array we show on screen from the float array we do
	// our rendering into
	xen::RawImage raw_image;
	raw_image.size   = target.size;
	raw_image.pixels = (xen::Color*)target.pixels;
	xsr::getImageFromFramebuffer(&target, raw_image);

	//////////////////////////////////////////////////////////////////////////
	// Put the image on screen

	// Note the -target.height, this mirrors the image on y since our idea of
	// which way is up does not line up with windows
	StretchDIBits(window->context,                        //dest
	              0, (int)target.height, (int)target.width, -(int)target.height,  //dest region -> could impl black bars here if render target size != window size
	              0, 0, (int)target.width, (int)target.height,  //source region
	              target.pixels, &target.bitmap_info,           //source data
	              DIB_RGB_COLORS,                               //source color type (not palette)
	              SRCCOPY);                                     //replace dest with source, no blend

	return;
}

#endif
