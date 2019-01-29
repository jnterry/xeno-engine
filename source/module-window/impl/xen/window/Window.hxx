////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation details regarding the Window type
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_HXX
#define XEN_GRAPHICS_WINDOW_HXX

#include <xen/window/Window.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/RingBuffer.hpp>
#include <xen/config.hpp>

namespace xen {
	struct Window;
	struct ArenaLinear;

	namespace impl {
		struct WindowBase {
			/// \brief Bitfield representing misc state about the window
			enum StateFlags : u08 {
				/// \brief Whether the window is currently open
				IS_OPEN           = 0x01,

				/// \brief Whether the window is currently focused
				IS_FOCUSED        = 0x02,

				/// \brief Set when the mouse cursor is within the window's client
				/// area. WindowEvent::MouseEntered and WindowEvent::MouseLeft events
				/// should be generated when this value changes
				MOUSE_OVER_WINDOW = 0x04,
			};
			u08 state;

			/// \brief Queue of events to process for this window
			xen::RingBuffer<WindowEvent, false> events;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Pushes some event onto the event queue. Makes a copy of
		/// the specified event, thus caller retains ownership of the event instance
		/// If the queue is already full then overwrites the oldest event and
		/// returns false.
		/////////////////////////////////////////////////////////////////////
		bool pushEvent(Window* window, const WindowEvent& event);
	}
}

#if defined XEN_OS_UNIX
	#include "unix.hxx"
#elif defined XEN_OS_WINDOWS
	#include "win.hxx"
#endif

#endif
