////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the base RenderTargetImpl type and then
/// includes the platform specific header with the true type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGETIMPL_HXX
#define XEN_SREN_RENDERTARGETIMPL_HXX

#include <xen/graphics/Color.hpp>
#include <xen/core/array_types.hpp>
#include <xen/config.hpp>

namespace xen {
	struct Window;
	struct Allocator;

	namespace sren {

		struct RenderTargetImpl;

		// Disable gcc's warning about anonymous structs in unions temporarily...
		#pragma GCC diagnostic push
		#pragma GCC diagnostic ignored "-Wpedantic"

		struct RenderTargetImplBase {
			/// \brief The window this render target is for (or nullptr if an
			/// off screen render buffer)
			Window* window;

			/// \brief Pointer to first element of color buffer
			/// 2d array flattened into 1d as {x0y0, x1y0, y1x0, y1x1}
			Color4f* color;

			/// \brief Pointer to first element of depth buffer
			/// 2d array flattened into 1d as {x0y0, x1y0, y1x0, y1x1}
			float*   depth;

			union {
				struct {
					u32 width;
					u32 height;
				};
				Vec2u size;
			};
		};

		#pragma GCC diagnostic pop // re-enable -Wpedantic

		void doPlatformRenderTargetInit(xen::Allocator* alloc,
		                                RenderTargetImpl& target,
		                                Window* window);

		void doPlatformRenderTargetResize(xen::Allocator* alloc,
		                                  RenderTargetImpl& target,
		                                  Window* window);

		void doPlatformRenderTargetDestruction(xen::Allocator* alloc,
		                                       RenderTargetImpl& target,
		                                       Window* window);

		void presentRenderTarget(Window* window, RenderTargetImpl& target);
	}
}

#if XEN_OS_UNIX
	#include "RenderTargetImpl.unix.hxx"
#endif

#endif
