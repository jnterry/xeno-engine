////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the RenderTargetImpl type for windows
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_WIN_HXX
#define XEN_SREN_RENDERTARGETIMPL_WIN_HXX

#include "../windows_header.hxx"

namespace xen {
	namespace sren {
		struct RenderTargetImpl : public RenderTargetImplBase {
			/// \brief Array of pixel values to be presented on screen
			Color* pixels;

			///\ Info about the bitmap represented by pixels array
			BITMAPINFO bitmap_info;
		};
	}
}

#endif