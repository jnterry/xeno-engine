////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation details regarding the Window type
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_HXX
#define XEN_GRAPHICS_WINDOW_HXX

#include <xen/graphics/Window.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/RingBuffer.hpp>
#include <xen/config.hpp>

namespace xen {
	struct Window;
	struct ArenaLinear;

	namespace impl {
		struct WindowBase {
			/// \brief True when the mouse is within the window's client area
			/// WindowEvent::MouseEntered and WindowEvnet::MouseLeft events are
			/// generated when this value changes
			bool mouse_over_window;

			/// \brief Specifies whether a window is currently open
			bool is_open;

			/// \brief Queue of events to process for this window
			xen::RingBuffer<WindowEvent, false> events;

			/// \brief The render target representing the area of the window
			/// that may be drawn to
			RenderTarget render_target;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Pushes some event onto the event queue. Makes a copy of
		/// the specified event, thus caller retains ownership of the event instance
		/// If the queue is already full then overwrites the oldest event and
		/// returns false.
		/////////////////////////////////////////////////////////////////////
		bool pushEvent(Window* window, const WindowEvent& event);

		/////////////////////////////////////////////////////////////////////
		/// \brief Operating system specific function which interacts with the
		/// OS to update the window's event queue. Automatically called by pollEvent.
		/// If this is not called frequently enough the OS may think the application
		/// is not responding.
		/////////////////////////////////////////////////////////////////////
		void dispatchEvents(Window* window);


		Window* createWindow (xen::ArenaLinear& arena, Vec2u size, const char* title);

		void destroyWindow(xen::Window* window);
	}
}

#if defined XEN_OS_UNIX
	#include "Window.unix.hxx"
#elif defined XEN_OS_WINDOWS
	#include "Window.win.hxx"
#endif

#endif