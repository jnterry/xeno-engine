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
#include <xen/window/win.hxx>

namespace xen{
	struct Allocator;

	namespace impl {
		const char* const WINDOW_CLASS_NAME = "XenWindowClass";
		bool windowClassRegistered = false;

		void setModifierKeys(xen::ModifierKeyState& state, WPARAM wParam){
			//:TOOD: what about alt, system, caps lock etc???
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
			setModifierKeys(e.mouse_button.modifers, wParam);
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
			window->mouse_over_window = true;
			TRACKMOUSEEVENT mouse_track;
			mouse_track.cbSize = sizeof(TRACKMOUSEEVENT);
			mouse_track.dwFlags = TME_LEAVE;
			mouse_track.hwndTrack = window->handle;
			mouse_track.dwHoverTime = 1;
			if(!TrackMouseEvent(&mouse_track)){
				// :TODO: log warn
				printf("Failed to setup mouse tracking on window, MouseLeft events will not be generated, "
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
					e.resize.new_size = getClientAreaSize(w);
					break;
				case WM_CLOSE:
					e.type = xen::WindowEvent::Closed;
					break;
				case WM_SETFOCUS:
					e.type = xen::WindowEvent::GainedFocus;
					break;
				case WM_KILLFOCUS:
					e.type = xen::WindowEvent::LostFocus;
					break;
				case WM_MOUSEMOVE:
					if(!w->mouse_over_window){
						startMouseTracking(w);
						WindowEvent enter_event;
						enter_event.type = xen::WindowEvent::MouseEntered;
						w->mouse_over_window = true;
						xen::impl::pushEvent(w, enter_event);
					}
					e.type = xen::WindowEvent::MouseMoved;
					break;
				case WM_MOUSELEAVE:
					w->mouse_over_window = false;
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
					setModifierKeys(e.mouse_wheel.modifiers, keys);
					break;
				}
				case WM_KEYDOWN:
					if(lParam & 0x40000000){
						e.type = xen::WindowEvent::KeyRepeated;
					} else {
						e.type = xen::WindowEvent::KeyPressed;
					}
					e.key.key = xen::impl::xenKeyFromVirtualKey(wParam, lParam);
					//:TODO: find state of modifier keys (GetAsyncKeyState())
					break;
				case WM_KEYUP:
					e.type = xen::WindowEvent::KeyReleased;
					e.key.key = xen::impl::xenKeyFromVirtualKey(wParam, lParam);
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

		///< Registers the WNDCLASS with windows so that we can create instances
		bool registerWindowClass(HINSTANCE hInstance){
			WNDCLASS wc = {};
			wc.style = CS_OWNDC | CS_DBLCLKS | CS_HREDRAW | CS_VREDRAW;
			wc.lpfnWndProc = windowEventCallback;
			wc.hInstance = hInstance;
			wc.lpszClassName = WINDOW_CLASS_NAME;

			if (RegisterClass(&wc) == 0){
				return false;
			} else {
				windowClassRegistered = true;
				return true;
			}
		}

		void dispatchEvents(Window* w){
			if(w->handle == NULL){ return; }
			MSG msg;
			BOOL msgResult = PeekMessage(&msg, w->handle, 0, 0, PM_REMOVE);
			while (msgResult != 0){
				TranslateMessage(&msg);
				DispatchMessage(&msg);
				msgResult = PeekMessage(&msg, w->handle, 0, 0, PM_REMOVE);
			}
		}

		Window* createWindow(xen::ArenaLinear& arena, Vec2u size, const char* title){
			HINSTANCE module = GetModuleHandle(NULL);
			if(!impl::windowClassRegistered){
				if(impl::registerWindowClass(module)){
					// :TODO: log info
					printf("Registered Xeno Engine's window class successfully\n");
				} else {
					// :TODO: log
					// :TODO: FormatMessage lets you get a error string from a GetLastError, make some helper?
					printf("ERROR: Failed to register Xeno Engine's window class, GetLastError: %i\n",
					       GetLastError()
					       );
					return nullptr;
				}
			}

			//////////////////////////////////////////////////////////
			// Create the window
			void* oldNextByte = arena.next_byte; // :TODO: use memory transaction
			Window* result = xen::reserveType<xen::Window>(arena);
			*result = {};

		  result->events.elements = xen::reserveTypeArray<xen::WindowEvent>(arena, 64);
			result->events.capacity = 64;

			result->handle = CreateWindowEx(0,
			                                impl::WINDOW_CLASS_NAME,
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
				printf("ERROR: Failed to create a window, GetLastError: %i\n", GetLastError());
				arena.next_byte = oldNextByte;
				return nullptr;
			}
			//enable us to get the xen::Window* from the HWND, eg when handling event callbacks
			SetWindowLongPtr(result->handle, GWLP_USERDATA, (LONG_PTR)result);

			//////////////////////////////////////////////////////////
			// Setup the window's pixel format and get a device context
			result->context = GetDC(result->handle);
			if(result->context == NULL){
				printf("ERROR: Failed to create window since was unable to obtain a device context\n");
				arena.next_byte = oldNextByte;
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
				printf("Failed to create window as was unable to find a suitable pixel format supported by the hardware\n");
				arena.next_byte = oldNextByte;
				return nullptr;
			}

			PIXELFORMATDESCRIPTOR actualFormat;
			DescribePixelFormat(result->context, actualFormatIndex, sizeof(PIXELFORMATDESCRIPTOR), &actualFormat);
			if (!SetPixelFormat(result->context, actualFormatIndex, &actualFormat)){
				printf("Failed to create window as was unable to set its pixel format\n");
				arena.next_byte = oldNextByte; // :TDOO: use memory transaction
				return nullptr;
			}

			result->state |= xen::Window::IS_OPEN;

			return result;
		}

		//Doesn't deallocate any memory, closes + destroys window with window api
		void destroyWindow(Window* window){
			DestroyWindow(window->handle);
			*window = {};
		}
	} //end of namespace xen::impl::

	/// \brief Retrieves the size of the client area (ie, part that may be renderered on) of some window
	Vec2u getClientAreaSize(Window* window){
		Vec2u result = {};
		if(window->handle == NULL){ return result; }
		RECT rect;
		GetClientRect(window->handle, &rect);
		result.x = (u32)rect.right;
		result.y = (u32)rect.bottom;
		return result;
	}

	bool isKeyPressed(Key key, Window*){
		UINT vk = impl::virtualKeyFromXenKey(key);
		if(vk == 0){ return false; }
		return (GetAsyncKeyState((int)vk) & ~1) != 0; //well done microsoft, store virtual key as UINT usually, but then take ints sometimes...
	}

	void setWindowTitle(Window* window, const char* title){
		SetWindowText(window->handle, title);
	}
}

#endif
