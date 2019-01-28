////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of the window type, and associated functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_HPP
#define XEN_GRAPHICS_WINDOW_HPP

#include <xen/math/vector_types.hpp>
#include <xen/graphics/GraphicsHandles.hpp>

namespace xen {

	/// \brief Opaque type representing a Window - actual type depends on platform
	struct Window;

	/// \brief Retrieves the size of the client area (IE: part that may
	// be rendered to) of some window
	Vec2u getClientAreaSize(Window* window);

	/// \brief Updates the title of some window - typically displayed by the
	/// window manager above the client area
	void setWindowTitle(Window* window, const char* title);

	/// \brief Retrieves the RenderTarget representing the client area of
	/// some window
	RenderTarget getRenderTarget(Window* window);

	/// \brief Swaps the buffers of some Window such that the application
	/// can begin drawing to a buffer while the other is displayed. Nothing
	/// will be displayed on the window's surface until this function is called
	//void swapBuffers(Window* window);

	bool isWindowOpen(const Window* window);

	/// \brief Unsigned integer type which is capable of holding a bitwise
	/// combination the MouseButtons bit field values
	typedef u08 MouseButtonState;

	// :TODO: convert to xen::BitField
	struct MouseButtons{
		enum Values : MouseButtonState{
			Left    = 0x01,
			Right   = 0x02,
			Middle  = 0x04,
			Extra1  = 0x08,
			Extra2  = 0x10,
			Unknown = 0x20,
		};
	};

	/// \brief Unsigned integer type which is capable of holding a bitwise
	/// combination of the ModifierKeys bit field values
	typedef u08 ModifierKeyState;

	// :TODO: convert to xen::BitField
	struct ModifierKeys {
		enum Values {
			Alt     = 0x01,
			Shift   = 0x02,
			Control = 0x04,
			System  = 0x08,
		};
		//:TODO: num lock, scroll lock, caps lock ?
	};

	/// \brief Enumeration of the possible keyboard keys
	enum class Key{
		// Note: OS to xen::Key translation relies on these having sequential values,
		// starting with A
		A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,

		// Note: OS to xen::Key translation relies on these having sequential values,
		// starting with 0
		Num0, Num1, Num2, Num3, Num4, Num5, Num6, Num7, Num8, Num9,

		//Note: OS to xen::Key translation relies on these having sequential values,
		// starting with 0
		Numpad0, Numpad1, Numpad2, Numpad3, Numpad4,
		Numpad5, Numpad6, Numpad7, Numpad8, Numpad9,
		// (these dont need to be sequential)
		NumpadAdd, NumpadMinus, NumpadMultiply, NumpadDivide, NumpadReturn,

		/*NumpadReturn, NumpadPlus, NumpadMinus,
		  NumpadPeriod, NumpadFSlash, NumpadAsterix,*/

		Comma, Period, ForwardSlash, Backslash,
		Semicolon, Quote, Tilde, Return,
		Minus, Equals,
		Tab, LBracket, RBracket,
		Space,
		Menu, // between altgr and ctrl on qwerty keyboard

		ArrowUp, ArrowDown, ArrowLeft, ArrowRight,

		// Note: OS to xen::Key translation relies on there being at least 24 F keys,
		// and that they have sequential values starting with F1
		F1, F2, F3, F4, F5, F6, F7, F8, F9,
		F10, F11, F12, F13, F14, F15, F16, F17, F18, F19,
		F20, F21, F22, F23, F24,

		PrintScreen, Pause, Insert, Delete, Home, End, PageUp, PageDown,

		Escape, Backspace,

		LSystem, RSystem, LCtrl, RCtrl, LShift, RShift, LAlt, RAlt,

		Unknown,

		Count, ///< The number of defined keys
	};

	struct EventKey {
		/// \brief Which key was pressed/released
		Key key;

		/// \brief Which modifier keys were pressed at the time of the event
		ModifierKeyState modifiers;

		//:TODO: mouse position ?
	};

	/// \brief Contains data about the mouse wheel being moved
	struct EventMouseWheel{
		enum Wheels{
			Horizontal,
			Vertical,
		};

		Vec2s            position;  ///< position of mouse in pixels relative to top left of the window
		int              delta;     ///< The number of ticks the mouse wheel moved (:TODO: +ve is which dir?)
		Wheels           wheel;     ///< Which wheel was moved
		ModifierKeyState modifiers; ///< The state of the modifier keys when the mouse wheel was moved
		MouseButtonState buttons;   ///< The state of the mouse button s when the mouse wheel was moved
	};

	/// \brief Contains data about the mouse being moved, also contains data
	/// about which button was pressed while being moved to facilitate handling
	/// drag events without the application keeping track of the mouse button
	/// state
	struct EventMouseMoved{
		Vec2s position; ///< new position of mouse in pixels relative to the top left of the window's client area

		///< The button pressed while the move was occurring
		///< Note that pressing or releasing a key while the mouse is in motion
		///< will cause separate EventMouseMoved instances to be made
		MouseButtonState button;
		ModifierKeyState modifers; ///< The state of the modifier keys when the mouse was being moved
	};

	/// \brief Contains data about a mouse button being pressed or released
	struct EventMouseButton{
		Vec2s position; 	         ///< position of mouse relative to top left of window, unit is pixels
		MouseButtons::Values button; ///< The button that was pressed or released
		ModifierKeyState modifers;   ///< The state of the modifier keys when the button was pressed/released
	};

	struct EventResized {
		Vec2u new_size;

		// :TODO: old size?
	};

	/// \brief Stores information about user input related to some Window instance
	struct WindowEvent{
		enum Type{
			Closed,
			GainedFocus,
			LostFocus,

			///< The mouse entered the client area of the window, no additional data
			MouseEntered,

			///< The mouse left the client area of the window, no additional data
			MouseLeft,

			MouseButtonPressed,
			MouseButtonReleased,
			MouseButtonDoubleClick,
			MouseMoved,
			MouseWheel,

			///< The window's client area size was changed, data in .resize
			Resized,

			///< A keyboard key was pressed, data in .key
			KeyPressed,

			///< A keyboard key was held and the OS's key repeat timer is generating another message
			KeyRepeated,

			///< A keyboard key was released, data in .key
			KeyReleased,

			//:TODO: text typed event (returns char rather than key code, takes into account shift to get typed char, uses key repeat)

			Count,
		};

		Type type; ///< The type of the event

		/// \brief Whether the window had focus when this event was produced
		bool has_focus;

		///< Extra data, which is set depends on the event's type
		union{
			EventMouseButton mouse_button;
			EventMouseMoved  mouse_moved;
			EventMouseWheel  mouse_wheel;
			EventKey         key;
			EventResized     resize;
		};
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves a pointer to the next event in the event queue of a
	/// window, or null pointer if there are no more events at the current time
	///
	/// \note This pointer refers to memory owned by the window system and no
	/// attempt should be made to free it. The pointer is guarenteed to remain
	/// valid until the next call to pollEvent for the same window
	/////////////////////////////////////////////////////////////////////
	WindowEvent* pollEvent(Window* window);

	/// \brief Determines if a keyboard key is currently pressed
	/// \todo :TODO: only reason we need to pass a Window in is that on unix
	/// we need a display connection -> turn window management into a reloadable
	/// module so we can store some global state
	bool isKeyPressed(Key key, Window* window);
}

#endif
