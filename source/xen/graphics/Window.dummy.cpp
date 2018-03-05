////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file window.dummy.cpp
/// \author Jamie Terry
/// \date 2017/05/22
/// \brief Contains dummy window implementation that wont actually open a window
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_DUMMY_CPP
#define XEN_GRAPHICS_WINDOW_DUMMY_CPP

#include <xen/math/vector.hpp>
#include <xen/graphics/Graphics_types.hpp>

namespace xen{
	struct Allocator;

	struct Window : public xen::impl::WindowBase{

	};

	namespace impl{
		void dispatchEvents(Window* w){
			// no-op
		}
	} //end of namespace xen::impl::

	bool isWindowOpen(Window* window){
		return true;
	}

	Vec2u getClientAreaSize(Window* window){
		Vec2u result = {0, 0};
		return result;
	}

	bool isKeyPressed(Key key){
		return false;
	}

	void setWindowTitle(Window* window, const char* title){
		// no-op
	}

	RenderTarget getRenderTarget(Window* window){
		return xen::makeNullHandle<RenderTarget>();
	}

	void swapBuffers(Window* window){
		// no-op
	}
}

#endif
