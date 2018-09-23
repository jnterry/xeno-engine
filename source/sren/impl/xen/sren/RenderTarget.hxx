////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the base RenderTarget type and then
/// includes the platform specific header with the true type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RENDERTARGET_HXX
#define XEN_SREN_RENDERTARGET_HXX

#include <xen/sren/Framebuffer.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/core/array_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/config.hpp>

#include <thpool.h>

namespace xen {
	struct Window;
	struct Allocator;
}

namespace xsr {
	struct RenderTarget;

	struct RenderTargetBase : public xsr::Framebuffer {
		/// \brief The window this render target is for (or nullptr if an
		/// off screen render buffer)
		xen::Window* window;
	};

	void doPlatformRenderTargetInit(xen::Allocator* alloc,
	                                xsr::RenderTarget& target,
	                                xen::Window* window);

	void doPlatformRenderTargetResize(xen::Allocator* alloc,
	                                  xsr::RenderTarget& target,
	                                  xen::Window* window);

	void doPlatformRenderTargetDestruction(xen::Allocator* alloc,
	                                       xsr::RenderTarget& target,
	                                       xen::Window* window);

	void presentRenderTarget(xen::Window* window, xsr::RenderTarget& target, threadpool thpool);

	/////////////////////////////////////////////////////////////////////
	/// \brief Clears the diffuse component of a render target to the
	/// specified color
	/////////////////////////////////////////////////////////////////////
	void clear(xsr::RenderTarget& target, xen::Color color);

	/////////////////////////////////////////////////////////////////////
	/// \brief Clears the diffuse component of a section of a render target
	/// to the specified color
	/////////////////////////////////////////////////////////////////////
	void clear(xsr::RenderTarget& target, const xen::Aabb2u& viewport, xen::Color color);
}

#ifdef XEN_OS_UNIX
	#include "RenderTarget.unix.hxx"
#elif defined XEN_OS_WINDOWS
	#include "RenderTarget.win.hxx"
#else
	#error "Software renderer not supported on this platform!"
#endif

#endif
