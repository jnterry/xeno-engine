////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains windows specific implementations of functions for dealing
/// with xen::Window
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_WIN_CPP
#define XEN_GRAPHICS_WINDOW_WIN_CPP

#include <xen/math/vector_types.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/bits.hpp>
#include <xen/core/array.hpp>
#include <xen/kernel/log.hpp>
#include <xen/kernel/Kernel.hpp>
#include <xen/window/win.hxx>

static const constexpr char* const WINDOW_CLASS_NAME = "XenWindowClass";

// Max number of windows that can be opened and managed by this module
static const constexpr u64 MAX_WINDOWS = 32;

// Number of events that can be in each window's queue
static const constexpr u64 EVENT_QUEUE_LENGTH = 64;

namespace xwn {
	struct State {
		xen::ModuleApiWindow api;

		// List of active windows
		xen::StretchyArray<xen::Window> windows;
	};
	State* state;

	Vec2u getClientAreaSize(xen::Window* window){
		RECT rect;
		if(GetClientRect(window->handle, &rect)){
			return { (u32)rect.right, (u32)rect.bottom };
		} else {
			return Vec2u::Origin;
		}
	}
}

namespace xen{
	struct Allocator;

	namespace impl {
		void setModifierKeys(xen::ModifierKeyState& state, WPARAM wParam){
			//:TODO: what about alt, system, caps lock etc???
			if(wParam & MK_CONTROL) { state |= xen::ModifierKeys::Control; }
			if(wParam & MK_SHIFT)   { state |= xen::ModifierKeys::Shift;   }
		}

		void setMousePosition(Vec2s& dest, LPARAM lParam){
			dest.x = GET_X_LPARAM(lParam);
			dest.y = GET_Y_LPARAM(lParam);
		}

		void setMouseButton(WindowEvent& e, xen::WindowEvent::Type type, xen::MouseButtons::Values button, WPARAM wParam, LPARAM lParam){
			e.type = type;
			if(button == xen::MouseButtons::Extra1 && GET_XBUTTON_WPARAM(wParam) == XBUTTON2){
				button = xen::MouseButtons::Extra2;
			}
			e.mouse_button.button = button;
			setModifierKeys (e.modifiers, wParam);
			setMousePosition(e.mouse_button.position, lParam);
		}

		Key xenKeyFromVirtualKey(WPARAM wParam, LPARAM lParam){
			if (wParam >= 'A' && wParam <= 'Z') {
				return static_cast<xen::Key>((u32)Key::A + (wParam - 'A'));
			} else if (wParam >= '0' && wParam <= '9') {
				return static_cast<xen::Key>((u32)Key::Num0 + (wParam - '0'));
			} else if (wParam >= VK_F1 && wParam <= VK_F24) {
				return static_cast<xen::Key>((u32)Key::F1 + (wParam - VK_F1));
			} else if (wParam >= VK_NUMPAD0 && wParam <= VK_NUMPAD9) {
				return static_cast<xen::Key>((u32)Key::Numpad0 + (wParam - VK_NUMPAD0));
			} else {
				switch(wParam){
				case VK_OEM_COMMA:  return xen::Key::Comma;
				case VK_OEM_PERIOD: return xen::Key::Period;
				case VK_OEM_2:      return xen::Key::ForwardSlash;
				case VK_OEM_5:      return xen::Key::Backslash;
				case VK_OEM_3:      return xen::Key::Quote;
				case VK_OEM_7:      return xen::Key::Tilde;
				case VK_RETURN:     return lParam & 0x1000000 ? xen::Key::NumpadReturn : xen::Key::Return;
				case VK_OEM_MINUS:  return xen::Key::Minus;
				case VK_OEM_PLUS:   return xen::Key::Equals;
				case VK_TAB:        return xen::Key::Tab;
				case VK_OEM_4:      return xen::Key::LBracket;
				case VK_OEM_6:      return xen::Key::RBracket;
				case VK_UP:         return xen::Key::ArrowUp;
				case VK_DOWN:       return xen::Key::ArrowDown;
				case VK_LEFT:       return xen::Key::ArrowLeft;
				case VK_RIGHT:      return xen::Key::ArrowRight;
				case VK_SNAPSHOT:   return xen::Key::PrintScreen;
				case VK_PAUSE:      return xen::Key::Pause;
				case VK_HOME:       return xen::Key::Home;
				case VK_END:        return xen::Key::End;
				case VK_PRIOR:      return xen::Key::PageUp;
				case VK_NEXT:       return xen::Key::PageDown;
				case VK_LWIN:       return xen::Key::LSystem;
				case VK_RWIN:       return xen::Key::RSystem;
				case VK_CONTROL:    return lParam & 0x01000000 ? xen::Key::RCtrl : xen::Key::LCtrl;
				case VK_MENU:       return lParam & 0x01000000 ? xen::Key::RAlt : xen::Key::LAlt;
				case VK_SHIFT:{
					UINT new_vk = MapVirtualKey((UINT)((lParam & 0x00ff0000) >> 16), MAPVK_VSC_TO_VK_EX);
					return new_vk == VK_RSHIFT ? xen::Key::RShift : xen::Key::LShift;
				}
				default: return xen::Key::Unknown;
				}
			}
		}

		UINT virtualKeyFromXenKey(Key key){
			u32 value = (u32)key;
			if(value >= (u32)Key::A && value <= (u32)Key::Z){
				return 'A' + (value - (u32)Key::A);
			} else if(value >= (u32)Key::Num0 && value <= (u32)Key::Num9){
				return '0' + (value - (u32)Key::Num0);
			} else if(value >= (u32)Key::F1 && value <= (u32)Key::F24){
				return VK_F1 + (value - (u32)Key::F1);
			} else if(value >= (u32)Key::Numpad0 && value <= (u32)Key::Numpad9){
				return VK_NUMPAD0 + (value - (u32)Key::Numpad0);
			} else {
				switch(key){
				case xen::Key::Comma:        return VK_OEM_COMMA;
				case xen::Key::Period:       return VK_OEM_PERIOD;
				case xen::Key::ForwardSlash: return VK_OEM_2;
				case xen::Key::Backslash:    return VK_OEM_5;
				case xen::Key::Quote:        return VK_OEM_3;
				case xen::Key::Tilde:        return VK_OEM_7;
				case xen::Key::Return:       return VK_RETURN;
				case xen::Key::Minus:        return VK_OEM_MINUS;
				case xen::Key::Equals:       return VK_OEM_PLUS;
				case xen::Key::Tab:          return VK_TAB;
				case xen::Key::LBracket:     return VK_OEM_4;
				case xen::Key::RBracket:     return VK_OEM_6;
				case xen::Key::ArrowUp:      return VK_UP;
				case xen::Key::ArrowDown:    return VK_DOWN;
				case xen::Key::ArrowLeft:    return VK_LEFT;
				case xen::Key::ArrowRight:   return VK_RIGHT;
				case xen::Key::PrintScreen:  return VK_SNAPSHOT;
				case xen::Key::Pause:        return VK_PAUSE;
				case xen::Key::Home:         return VK_HOME;
				case xen::Key::End:          return VK_END;
				case xen::Key::PageUp:       return VK_PRIOR;
				case xen::Key::PageDown:     return VK_NEXT;
				case xen::Key::LSystem:      return VK_LWIN;
				case xen::Key::RSystem:      return VK_RWIN;
				case xen::Key::LCtrl:        return VK_LCONTROL;
				case xen::Key::RCtrl:        return VK_RCONTROL;
				case xen::Key::LAlt:         return VK_LMENU;
				case xen::Key::RAlt:         return VK_RMENU;
				case xen::Key::LShift:       return VK_LSHIFT;
				case xen::Key::RShift:       return VK_RSHIFT;
				default:                     return 0;
				}
			}
		}

		void startMouseTracking(Window* window){
			window->state |= xen::Window::MOUSE_OVER_WINDOW;
			TRACKMOUSEEVENT mouse_track;
			mouse_track.cbSize = sizeof(TRACKMOUSEEVENT);
			mouse_track.dwFlags = TME_LEAVE;
			mouse_track.hwndTrack = window->handle;
			mouse_track.dwHoverTime = 1;
			if(!TrackMouseEvent(&mouse_track)){
				XenLogWarn("Failed to setup mouse tracking on window, MouseLeft events will not be generated, "
				           "GetLastError: %i\n", GetLastError());
			}
		}

		LRESULT CALLBACK windowEventCallback(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam){
			LRESULT result = 0;
			xen::Window* w = (xen::Window*)GetWindowLongPtr(hwnd, GWLP_USERDATA);
			if (w == NULL){
				//then this is a msg being sent during the CreateWindowEx call, hence the user data
				//hasent been set to the xen::Window pointer, defer to windows
				result = DefWindowProc(hwnd, msg, wParam, lParam);
			} else {
				xen::WindowEvent e;
				bool valid_event = true;
				switch (msg){
				case WM_SIZE:
					e.type = xen::WindowEvent::Resized;
					e.resize.old_size = w->size;
					e.resize.new_size = xwn::getClientAreaSize(w);
					w->size           = e.resize.new_size;
					break;
				case WM_CLOSE:
					e.type = xen::WindowEvent::Closed;
					break;
				case WM_SETFOCUS:
					xen::setBits(w->state, (u08)xen::Window::HAS_FOCUS);
					e.type = xen::WindowEvent::GainedFocus;
					break;
				case WM_KILLFOCUS:
					xen::clearBits(w->state, (u08)xen::Window::HAS_FOCUS);
					e.type = xen::WindowEvent::LostFocus;
					break;
				case WM_MOUSEMOVE:
					if(!w->state & xen::Window::MOUSE_OVER_WINDOW){
						startMouseTracking(w);
						WindowEvent enter_event;
						enter_event.type = xen::WindowEvent::MouseEntered;
						w->state |= xen::Window::MOUSE_OVER_WINDOW;
						xen::impl::pushEvent(w, enter_event);
					}
					e.type = xen::WindowEvent::MouseMoved;
					break;
				case WM_MOUSELEAVE:
					xen::clearBits(w->state, (u08)xen::Window::MOUSE_OVER_WINDOW);
					e.type = xen::WindowEvent::MouseLeft;
					break;
				case WM_LBUTTONDOWN:
					setMouseButton(e, xen::WindowEvent::MouseButtonPressed, xen::MouseButtons::Left, wParam, lParam);
					break;
				case WM_LBUTTONUP:
					setMouseButton(e, xen::WindowEvent::MouseButtonReleased, xen::MouseButtons::Left, wParam, lParam);
					break;
				case WM_LBUTTONDBLCLK:
					setMouseButton(e, xen::WindowEvent::MouseButtonDoubleClick, xen::MouseButtons::Left, wParam, lParam);
					break;
				case WM_RBUTTONDOWN:
					setMouseButton(e, xen::WindowEvent::MouseButtonPressed, xen::MouseButtons::Right, wParam, lParam);
					break;
				case WM_RBUTTONUP:
					setMouseButton(e, xen::WindowEvent::MouseButtonReleased, xen::MouseButtons::Right, wParam, lParam);
					break;
				case WM_RBUTTONDBLCLK:
					setMouseButton(e, xen::WindowEvent::MouseButtonDoubleClick, xen::MouseButtons::Right, wParam, lParam);
					break;
				case WM_MBUTTONDOWN:
					setMouseButton(e, xen::WindowEvent::MouseButtonPressed, xen::MouseButtons::Middle, wParam, lParam);
					break;
				case WM_MBUTTONUP:
					setMouseButton(e, xen::WindowEvent::MouseButtonReleased, xen::MouseButtons::Middle, wParam, lParam);
					break;
				case WM_MBUTTONDBLCLK:
					setMouseButton(e, xen::WindowEvent::MouseButtonDoubleClick, xen::MouseButtons::Middle, wParam, lParam);
					break;
				case WM_XBUTTONDOWN:
					setMouseButton(e, xen::WindowEvent::MouseButtonPressed, xen::MouseButtons::Extra1, wParam, lParam);
					break;
				case WM_XBUTTONUP:
					setMouseButton(e, xen::WindowEvent::MouseButtonReleased, xen::MouseButtons::Extra1, wParam, lParam);
					break;
				case WM_XBUTTONDBLCLK:
					setMouseButton(e, xen::WindowEvent::MouseButtonDoubleClick, xen::MouseButtons::Extra1, wParam, lParam);
					break;
				case WM_MOUSEWHEEL:{
					e.type = xen::WindowEvent::MouseWheel;
					e.mouse_wheel.delta = GET_WHEEL_DELTA_WPARAM(wParam) / WHEEL_DELTA;
					auto keys = GET_KEYSTATE_WPARAM(wParam);
					if(keys & MK_LBUTTON) { e.mouse_wheel.buttons |= xen::MouseButtons::Left;   }
					if(keys & MK_MBUTTON) { e.mouse_wheel.buttons |= xen::MouseButtons::Middle; }
					if(keys & MK_RBUTTON) { e.mouse_wheel.buttons |= xen::MouseButtons::Right;  }
					if(keys & MK_XBUTTON1){ e.mouse_wheel.buttons |= xen::MouseButtons::Extra1; }
					if(keys & MK_XBUTTON2){ e.mouse_wheel.buttons |= xen::MouseButtons::Extra2; }
					setModifierKeys(e.modifiers, keys);
					break;
				}
				case WM_KEYDOWN:
					if(lParam & 0x40000000){
						e.type = xen::WindowEvent::KeyRepeated;
					} else {
						e.type = xen::WindowEvent::KeyPressed;
					}
					e.key = xen::impl::xenKeyFromVirtualKey(wParam, lParam);
					//:TODO: find state of modifier keys (GetAsyncKeyState())
					break;
				case WM_KEYUP:
					e.type = xen::WindowEvent::KeyReleased;
					e.key = xen::impl::xenKeyFromVirtualKey(wParam, lParam);
					//:TODO: find state of modifier keys (GetAsyncKeyState())
					break;
				default: //some other msg we don't care about, defer to windows
					result = DefWindowProc(hwnd, msg, wParam, lParam);
					valid_event = false;
					break;
				}
				if(valid_event){
					xen::impl::pushEvent(w, e);
				}
			}
			return result;
		}

		Window* createWindow(Vec2u size, const char* title){
			if(xwn::state->windows.size >= xwn::state->windows.capacity){
				XenLogError("Cannot create window as reached xenogin active window limit");
				return nullptr;
			}

			Window* result = &xwn::state->windows[xwn::state->windows.size++];
			*result = {};

			HINSTANCE module = GetModuleHandle(NULL);

			//////////////////////////////////////////////////////////
			// Create the window
			result->handle = CreateWindowEx(0,
			                                WINDOW_CLASS_NAME,
			                                title,
			                                WS_OVERLAPPEDWINDOW | WS_VISIBLE,
			                                CW_USEDEFAULT,
			                                CW_USEDEFAULT,
			                                size.x,
			                                size.y,
			                                0,
			                                0,
			                                module,
			                                0);

			if(result->handle == nullptr){
			  XenLogError("Failed to create a window, GetLastError: %i", GetLastError());
				return nullptr;
			}
			//enable us to get the xen::Window* from the HWND, eg when handling event callbacks
			SetWindowLongPtr(result->handle, GWLP_USERDATA, (LONG_PTR)result);

			//////////////////////////////////////////////////////////
			// Setup the window's pixel format and get a device context
			result->context = GetDC(result->handle);
			if(result->context == NULL){
				XenLogError("Failed to create window since was unable to obtain a device context");
				return nullptr;
			}
			//setup pixel format
			//https://msdn.microsoft.com/en-us/library/windows/desktop/dd368826(v=vs.85).aspx
			//:TODO: make this configurable with params to the function?
			//ie, if we need a depth buffer, SUPPORT_OPENGL need not be set if planning on using directx, software renderer, etc
			PIXELFORMATDESCRIPTOR desiredPixelFormat = {
				sizeof(PIXELFORMATDESCRIPTOR),
				1, //version, always 1
				PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,   //Flags
				PFD_TYPE_RGBA,            //The kind of framebuffer. RGBA or palette.
				32,                       //Colordepth of the framebuffer.
				0, 0, 0, 0, 0, 0,
				0,
				0,
				0,
				0, 0, 0, 0,
				24,                       //Number of bits for the depthbuffer
				8,                        //Number of bits for the stencilbuffer
				0,                        //Number of Aux buffers in the framebuffer (MSDN says not supported?)
				PFD_MAIN_PLANE,
				0,
				0, 0, 0};
			//get index of closest match that the hardware supports
			int actualFormatIndex = ChoosePixelFormat(result->context, &desiredPixelFormat);
			if(actualFormatIndex == 0){
			  XenLogError("Failed to create window as was unable to find a suitable pixel format supported by the hardware");
				return nullptr;
			}

			PIXELFORMATDESCRIPTOR actualFormat;
			DescribePixelFormat(result->context, actualFormatIndex, sizeof(PIXELFORMATDESCRIPTOR), &actualFormat);
			if (!SetPixelFormat(result->context, actualFormatIndex, &actualFormat)){
				XenLogError("Failed to create window as was unable to set its pixel format");
				return nullptr;
			}

			result->state |= xen::Window::IS_OPEN | xen::Window::HAS_FOCUS;
			result->size = size;
			result->events.elements = (xen::WindowEvent*)xen::kernelAlloc(
		  	sizeof(xen::WindowEvent) * EVENT_QUEUE_LENGTH
		  );
			result->events.capacity = EVENT_QUEUE_LENGTH;

			return result;
		}

		void destroyWindow(xen::Window* window){
			u64 window_index = (
				(u64)(window - xwn::state->windows.elements) / sizeof(xen::Window)
			);
			if(window_index > xwn::state->windows.size){
				XenLogError("destroyWindow called with pointer to unknown window");
				*window = {};
				return;
			}

			xen::kernelFree(window->events.elements);
			DestroyWindow(window->handle);
			*window = {};

			xen::removeUnordered(xwn::state->windows, window_index);
		}
	} //end of namespace xen::impl::

	bool isKeyPressed(Key key){
		UINT vk = impl::virtualKeyFromXenKey(key);
		if(vk == 0){ return false; }
		return (GetAsyncKeyState((int)vk) & ~1) != 0; //well done microsoft, store virtual key as UINT usually, but then take ints sometimes...
	}

	void setWindowTitle(Window* window, const char* title){
		SetWindowText(window->handle, title);
	}
}



///////////////////////////////////////////////////////////////////////////////////////

void* init(const void* params){
	//////////////////////////////////////////////////////////////
	// Allocate window storage
	xwn::state = (xwn::State*)xen::kernelAlloc(
		sizeof(xwn::State) +
		sizeof(xen::Window) * MAX_WINDOWS +
		alignof(xen::Window)
	);
	xen::clearToZero(xwn::state);
	xwn::state->windows.elements = (xen::Window*)(
		xen::ptrGetAlignedForward(
			xen::ptrGetAdvanced(xwn::state, sizeof(xwn::State)), alignof(xen::Window)
		)
	);
	xwn::state->windows.capacity = MAX_WINDOWS;
	//////////////////////////////////////////////////////////////

	//////////////////////////////////////////////////////////////
	// Register "window class"
	HINSTANCE module = GetModuleHandle(NULL);
	WNDCLASS wc = {};
	wc.style = CS_OWNDC | CS_DBLCLKS | CS_HREDRAW | CS_VREDRAW;
	wc.lpfnWndProc = xen::impl::windowEventCallback;
	wc.hInstance = module;
	wc.lpszClassName = WINDOW_CLASS_NAME;

	if (RegisterClass(&wc) == 0){
		// :TODO: FormatMessage lets you get a error string from a GetLastError, make some helper?
		XenLogError("Failed to register Xeno Engine's window class, GetLastError: %i\n",
		            GetLastError()
		           );
		return nullptr;
	} else {
		XenLogDone("Registered Xeno Engine's window class successfully");
	}
	//////////////////////////////////////////////////////////////

	return xwn::state;
}

void shutdown(void* data, const void* params){
	for(int i = xwn::state->windows.size-1; i >= 0; --i){
		xen::impl::destroyWindow(&xwn::state->windows[i]);
	}

	xen::kernelFree(xwn::state);
	xwn::state = nullptr;
}

void* load( void* data, const void* params){
	xwn::state = (xwn::State*)data;

	xwn::state->api.getClientAreaSize = &xwn::getClientAreaSize;
	xwn::state->api.setWindowTitle    = &xen::setWindowTitle;
	xwn::state->api.isWindowOpen      = &xen::isWindowOpen;
	xwn::state->api.hasFocus          = &xen::hasFocus;
	xwn::state->api.pollEvent         = &xen::pollEvent;
	xwn::state->api.isKeyPressed      = &xen::isKeyPressed;
	xwn::state->api.createWindow      = &xen::impl::createWindow;
	xwn::state->api.destroyWindow     = &xen::impl::destroyWindow;

	return &xwn::state->api;
}

void tick(const xen::TickContext& tick){
	//////////////////////////////////////////////////////////////////
	// Process window events
	for(uint i = 0; i < xwn::state->windows.size; ++i){
		xen::Window* win = &xwn::state->windows[i];
	  MSG msg;
	  BOOL msgResult = PeekMessage(&msg, win->handle, 0, 0, PM_REMOVE);
	  while (msgResult != 0){
		  TranslateMessage(&msg);
		  DispatchMessage(&msg);
		  msgResult = PeekMessage(&msg, win->handle, 0, 0, PM_REMOVE);
	  }
  }
}

XenDeclareModule("window", &init, &shutdown, &load, nullptr, &tick)

#endif
