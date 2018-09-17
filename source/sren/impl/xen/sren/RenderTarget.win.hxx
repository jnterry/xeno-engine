////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the RenderTargetImpl type for windows
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_WIN_HXX
#define XEN_SREN_RENDERTARGETIMPL_WIN_HXX

#include <xen/windows_header.hxx>

namespace xsr {
	struct RenderTarget : public RenderTargetBase {
		/// \brief Array of pixel values to be presented on screen
		Color* pixels;

		///\ Info about the bitmap represented by pixels array
		BITMAPINFO bitmap_info;
	};
}

#endif
