////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform specific unix window implementation
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_WINDOW_UNIX_CPP
#define XEN_GRAPHICS_WINDOW_UNIX_CPP

#include <xen/graphics/Window.hpp>
#include <xen/math/vector.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

#include <xen/graphics/Window.unix.hxx>

namespace {
	xen::Key xenKeyFromKeysym(KeySym symbol){
		switch(symbol){
		case XK_Shift_L:      return xen::Key::LShift;
		case XK_Shift_R:      return xen::Key::RShift;
		case XK_Control_L:    return xen::Key::LCtrl;
		case XK_Control_R:    return xen::Key::RCtrl;
		case XK_Alt_L:        return xen::Key::LAlt;
		case XK_Alt_R:        return xen::Key::RAlt;
		case XK_Super_L:      return xen::Key::LSystem;
		case XK_Super_R:      return xen::Key::RSystem;
		case XK_Menu:         return xen::Key::Menu;
		case XK_Escape:       return xen::Key::Escape;
		case XK_semicolon:    return xen::Key::Semicolon;
		case XK_slash:        return xen::Key::ForwardSlash;
		case XK_equal:        return xen::Key::Equals;
		case XK_minus:        return xen::Key::Minus;
		case XK_bracketleft:  return xen::Key::LBracket;
		case XK_bracketright: return xen::Key::RBracket;
		case XK_comma:        return xen::Key::Comma;
		case XK_period:       return xen::Key::Period;
		case XK_apostrophe:   return xen::Key::Quote;
		case XK_backslash:    return xen::Key::Backslash;
		case XK_grave:        return xen::Key::Tilde;
		case XK_space:        return xen::Key::Space;
		case XK_Return:       return xen::Key::Return;
		case XK_BackSpace:    return xen::Key::Backspace;
		case XK_Tab:          return xen::Key::Tab;
		case XK_Prior:        return xen::Key::PageUp;
		case XK_Next:         return xen::Key::PageDown;
		case XK_End:          return xen::Key::End;
		case XK_Home:         return xen::Key::Home;
		case XK_Insert:       return xen::Key::Insert;
		case XK_Delete:       return xen::Key::Delete;
		case XK_Pause:        return xen::Key::Pause;
		case XK_F1:           return xen::Key::F1;
		case XK_F2:           return xen::Key::F2;
		case XK_F3:           return xen::Key::F3;
		case XK_F4:           return xen::Key::F4;
		case XK_F5:           return xen::Key::F5;
		case XK_F6:           return xen::Key::F6;
		case XK_F7:           return xen::Key::F7;
		case XK_F8:           return xen::Key::F8;
		case XK_F9:           return xen::Key::F9;
		case XK_F10:          return xen::Key::F10;
		case XK_F11:          return xen::Key::F11;
		case XK_F12:          return xen::Key::F12;
		case XK_F13:          return xen::Key::F13;
		case XK_F14:          return xen::Key::F14;
		case XK_F15:          return xen::Key::F15;
		case XK_Left:         return xen::Key::ArrowLeft;
		case XK_Right:        return xen::Key::ArrowRight;
		case XK_Up:           return xen::Key::ArrowUp;
		case XK_Down:         return xen::Key::ArrowDown;
		case XK_KP_Insert:    return xen::Key::Numpad0;
		case XK_KP_End:       return xen::Key::Numpad1;
		case XK_KP_Down:      return xen::Key::Numpad2;
		case XK_KP_Page_Down: return xen::Key::Numpad3;
		case XK_KP_Left:      return xen::Key::Numpad4;
		case XK_KP_Begin:     return xen::Key::Numpad5;
		case XK_KP_Right:     return xen::Key::Numpad6;
		case XK_KP_Home:      return xen::Key::Numpad7;
		case XK_KP_Up:        return xen::Key::Numpad8;
		case XK_KP_Page_Up:   return xen::Key::Numpad9;
		case XK_KP_Add:       return xen::Key::NumpadAdd;
		case XK_KP_Subtract:  return xen::Key::NumpadMinus;
		case XK_KP_Multiply:  return xen::Key::NumpadMultiply;
		case XK_KP_Divide:    return xen::Key::NumpadDivide;
		case XK_KP_Enter:     return xen::Key::NumpadReturn;
		case XK_a:            return xen::Key::A;
		case XK_b:            return xen::Key::B;
		case XK_c:            return xen::Key::C;
		case XK_d:            return xen::Key::D;
		case XK_e:            return xen::Key::E;
		case XK_f:            return xen::Key::F;
		case XK_g:            return xen::Key::G;
		case XK_h:            return xen::Key::H;
		case XK_i:            return xen::Key::I;
		case XK_j:            return xen::Key::J;
		case XK_k:            return xen::Key::K;
		case XK_l:            return xen::Key::L;
		case XK_m:            return xen::Key::M;
		case XK_n:            return xen::Key::N;
		case XK_o:            return xen::Key::O;
		case XK_p:            return xen::Key::P;
		case XK_q:            return xen::Key::Q;
		case XK_r:            return xen::Key::R;
		case XK_s:            return xen::Key::S;
		case XK_t:            return xen::Key::T;
		case XK_u:            return xen::Key::U;
		case XK_v:            return xen::Key::V;
		case XK_w:            return xen::Key::W;
		case XK_x:            return xen::Key::X;
		case XK_y:            return xen::Key::Y;
		case XK_z:            return xen::Key::Z;
		case XK_0:            return xen::Key::Num0;
		case XK_1:            return xen::Key::Num1;
		case XK_2:            return xen::Key::Num2;
		case XK_3:            return xen::Key::Num3;
		case XK_4:            return xen::Key::Num4;
		case XK_5:            return xen::Key::Num5;
		case XK_6:            return xen::Key::Num6;
		case XK_7:            return xen::Key::Num7;
		case XK_8:            return xen::Key::Num8;
		case XK_9:            return xen::Key::Num9;
		default:              return xen::Key::Unknown;
		}
	}

	KeySym xenKeysymFromKey(xen::Key key){
		switch(key){
		case xen::Key::LShift:         return XK_Shift_L;
		case xen::Key::RShift:         return XK_Shift_R;
		case xen::Key::LCtrl:          return XK_Control_L;
		case xen::Key::RCtrl:          return XK_Control_R;
		case xen::Key::LAlt:           return XK_Alt_L;
		case xen::Key::RAlt:           return XK_Alt_R;
		case xen::Key::LSystem:        return XK_Super_L;
		case xen::Key::RSystem:        return XK_Super_R;
		case xen::Key::Menu:           return XK_Menu;
		case xen::Key::Escape:         return XK_Escape;
		case xen::Key::Semicolon:      return XK_semicolon;
		case xen::Key::ForwardSlash:   return XK_slash;
		case xen::Key::Equals:         return XK_equal;
		case xen::Key::Minus:          return XK_minus;
		case xen::Key::LBracket:       return XK_bracketleft;
		case xen::Key::RBracket:       return XK_bracketright;
		case xen::Key::Comma:          return XK_comma;
		case xen::Key::Period:         return XK_period;
		case xen::Key::Quote:          return XK_apostrophe;
		case xen::Key::Backslash:      return XK_backslash;
		case xen::Key::Tilde:          return XK_grave;
		case xen::Key::Space:          return XK_space;
		case xen::Key::Return:         return XK_Return;
		case xen::Key::Backspace:      return XK_BackSpace;
		case xen::Key::Tab:            return XK_Tab;
		case xen::Key::PageUp:         return XK_Prior;
		case xen::Key::PageDown:       return XK_Next;
		case xen::Key::End:            return XK_End;
		case xen::Key::Home:           return XK_Home;
		case xen::Key::Insert:         return XK_Insert;
		case xen::Key::Delete:         return XK_Delete;
		case xen::Key::Pause:          return XK_Pause;
		case xen::Key::F1:             return XK_F1;
		case xen::Key::F2:             return XK_F2;
		case xen::Key::F3:             return XK_F3;
		case xen::Key::F4:             return XK_F4;
		case xen::Key::F5:             return XK_F5;
		case xen::Key::F6:             return XK_F6;
		case xen::Key::F7:             return XK_F7;
		case xen::Key::F8:             return XK_F8;
		case xen::Key::F9:             return XK_F9;
		case xen::Key::F10:            return XK_F10;
		case xen::Key::F11:            return XK_F11;
		case xen::Key::F12:            return XK_F12;
		case xen::Key::F13:            return XK_F13;
		case xen::Key::F14:            return XK_F14;
		case xen::Key::F15:            return XK_F15;
		case xen::Key::ArrowLeft:      return XK_Left;
		case xen::Key::ArrowRight:     return XK_Right;
		case xen::Key::ArrowUp:        return XK_Up;
		case xen::Key::ArrowDown:      return XK_Down;
		case xen::Key::Numpad0:        return XK_KP_Insert;
		case xen::Key::Numpad1:        return XK_KP_End;
		case xen::Key::Numpad2:        return XK_KP_Down;
		case xen::Key::Numpad3:        return XK_KP_Page_Down;
		case xen::Key::Numpad4:        return XK_KP_Left;
		case xen::Key::Numpad5:        return XK_KP_Begin;
		case xen::Key::Numpad6:        return XK_KP_Right;
		case xen::Key::Numpad7:        return XK_KP_Home;
		case xen::Key::Numpad8:        return XK_KP_Up;
		case xen::Key::Numpad9:        return XK_KP_Page_Up;
		case xen::Key::NumpadAdd:      return XK_KP_Add;
		case xen::Key::NumpadMinus:    return XK_KP_Subtract;
		case xen::Key::NumpadMultiply: return XK_KP_Multiply;
		case xen::Key::NumpadDivide:   return XK_KP_Divide;
		case xen::Key::NumpadReturn:   return XK_KP_Enter;
		case xen::Key::A:              return XK_a;
		case xen::Key::B:              return XK_b;
		case xen::Key::C:              return XK_c;
		case xen::Key::D:              return XK_d;
		case xen::Key::E:              return XK_e;
		case xen::Key::F:              return XK_f;
		case xen::Key::G:              return XK_g;
		case xen::Key::H:              return XK_h;
		case xen::Key::I:              return XK_i;
		case xen::Key::J:              return XK_j;
		case xen::Key::K:              return XK_k;
		case xen::Key::L:              return XK_l;
		case xen::Key::M:              return XK_m;
		case xen::Key::N:              return XK_n;
		case xen::Key::O:              return XK_o;
		case xen::Key::P:              return XK_p;
		case xen::Key::Q:              return XK_q;
		case xen::Key::R:              return XK_r;
		case xen::Key::S:              return XK_s;
		case xen::Key::T:              return XK_t;
		case xen::Key::U:              return XK_u;
		case xen::Key::V:              return XK_v;
		case xen::Key::W:              return XK_w;
		case xen::Key::X:              return XK_x;
		case xen::Key::Y:              return XK_y;
		case xen::Key::Z:              return XK_z;
		case xen::Key::Num0:           return XK_0;
		case xen::Key::Num1:           return XK_1;
		case xen::Key::Num2:           return XK_2;
		case xen::Key::Num3:           return XK_3;
		case xen::Key::Num4:           return XK_4;
		case xen::Key::Num5:           return XK_5;
		case xen::Key::Num6:           return XK_6;
		case xen::Key::Num7:           return XK_7;
		case xen::Key::Num8:           return XK_8;
		case xen::Key::Num9:           return XK_9;
		default:                       return 0;
		}
	}

	//////////////////////////////////////////////////////////////////////////
	/// \breif Converts an XKeyEvent keycode into a xen::Key
	//////////////////////////////////////////////////////////////////////////
	xen::Key xenKeyFromXKeyEvent(XKeyEvent* event){
		//:TODO: this impl taken from SFML, do we really need loop? - should be able to work
		// out modifier state from event, right?

		xen::Key result;

		// Try each KeySym index (modifier group) until we get a match
		for (int i = 0; i < 4; ++i){
			result = xenKeyFromKeysym(XLookupKeysym(event, i));

			if (result != xen::Key::Unknown) { break; }
		}

		return result;
	}

	//////////////////////////////////////////////////////////////////////////
	/// \breif Processes an XEvent, converting it into a xen::WindowEvent and
	/// adding it to the window's queue
	//////////////////////////////////////////////////////////////////////////
	void processEvent(xen::Window* w, XEvent* xe){
		xen::WindowEvent e;
		bool valid_event = true;
		switch(xe->type){
		case ClientMessage:{
			Atom wm_delete_window = XInternAtom(w->display, "WM_DELETE_WINDOW", False);
                        if((Atom)xe->xclient.data.l[0] == wm_delete_window){
				e.type = xen::WindowEvent::Closed;
			}
			break;
		} // end of case ClientMessage
		case KeyPress:
		case KeyRelease:{
			// :TODO: if release determine if actually released, or just a key repeat
			e.type = xe->type == KeyPress ? xen::WindowEvent::KeyPressed : xen::WindowEvent::KeyReleased;
			e.key.key = xenKeyFromXKeyEvent(&xe->xkey);
			break;
		} // end of case KeyPress/KeyRelease
		case ButtonPress:
		case ButtonRelease:{
			// x11 mouse buttons:
			// Button1 | 1 | left
			// Button2 | 2 | middle
			// Button3 | 3 | right
			// Button4 | 4 | scroll vertical up
			// Button5 | 5 | scroll vertical down
			//    ?    | 6 | scroll horizontal up
			//    ?    | 7 | scroll horizontal down
			//    ?    | 8 | extra 1 (back)
			//    ?    | 9 | extra 2 (forward)

			e.mouse_button.position.x = xe->xbutton.x;
			e.mouse_button.position.y = xe->xbutton.y;

			//:TODO: modifier keys
			//:TODO: handle double click events

			if(xe->xbutton.button >= Button4 && xe->xbutton.button <= 7){
				e.type = xen::WindowEvent::MouseWheel;
				e.mouse_wheel.delta = xe->xbutton.button & 1 ? 1 : -1;
				e.mouse_wheel.wheel = xe->xbutton.button > 5 ? xen::EventMouseWheel::Horizontal : xen::EventMouseWheel::Vertical;
			} else {
				e.type = xe->type == ButtonPress ? xen::WindowEvent::MouseButtonPressed : xen::WindowEvent::MouseButtonReleased;
				switch(xe->xbutton.button){
				case Button1: e.mouse_button.button = xen::MouseButtons::Left;    break;
				case Button2: e.mouse_button.button = xen::MouseButtons::Middle;  break;
				case Button3: e.mouse_button.button = xen::MouseButtons::Right;   break;
				case 8      : e.mouse_button.button = xen::MouseButtons::Extra1;  break;
				case 9      : e.mouse_button.button = xen::MouseButtons::Extra2;  break;
				default:      e.mouse_button.button = xen::MouseButtons::Unknown; break;
				}
			}
			break;
		} // end of case ButtonPress
		case ResizeRequest:{
			e.type = xen::WindowEvent::Resized;
			e.resize.new_size.x = xe->xresizerequest.width;
			e.resize.new_size.y = xe->xresizerequest.height;
			break;
		} // end of case ResizeRequest
		default:
			valid_event = false;
		}

		if(valid_event){
			xen::impl::pushEvent(w, e);
		}
	}

	int checkEventMatchesWindow(Display* disp, XEvent* event, XPointer user_data){
		return event->xany.window == (reinterpret_cast< ::Window >(user_data));
	}
}

namespace xen {
	struct Allocator;

	namespace impl{
		void dispatchEvents(Window* w){
			XEvent event;
			while(XCheckIfEvent(w->display, &event,
			                    &checkEventMatchesWindow,
			                    reinterpret_cast< XPointer >( w->xwindow)
			                    )
			      ){

				processEvent(w, &event);
			}
		}

		Window* createWindow(xen::ArenaLinear& arena, Vec2u size, const char* title){
				xen::MemoryTransaction transaction(arena);
				Window* result = xen::reserveType<Window>(arena);
				*result = {};

				result->events.elements = xen::reserveTypeArray<xen::WindowEvent>(arena, 64);
				result->events.capacity = 64;

				////////////////////////////////////////////////////////////////////////
				// Establish connection to x server, setup some parameters

				// :TODO: Best practices (I think) are that XOpenDisplay is only called
				// once and then multiple windows are opened with the same display - but
				// we can't store it as global state as this code may be compiled into
				// reloadable modules whose static state is lost upon reload -> maybe we
				// want to have a module-window that handles managing open windows?
				// The tick function could do event polling, etc
			  result->display = XOpenDisplay(NULL);
				if(result->display == nullptr){
					// :TODO: log error
					printf("ERROR: Failed to open x display\n");;
					return nullptr;
				} else {
					// :TODO: log
					printf("INFO: Opened x display successfully\n");
				}

				// Get window representing whole screen
				::Window root   = DefaultRootWindow(result->display);

				// Find an appropriate visual
				int color_depth  = 32;

				XVisualInfo vinfo;
				if(!XMatchVisualInfo(result->display,
				                     XDefaultScreen(result->display),
				                     color_depth,
				                     TrueColor,
				                     &vinfo)){
					printf("Cannot find 32bit truecolor visual\n");
					return nullptr;
				}
				Visual*  visual = vinfo.visual;

				XSetWindowAttributes    frame_attributes;
				frame_attributes.colormap = XCreateColormap(result->display,
				                                            root,
				                                            visual,
				                                            AllocNone);
				frame_attributes.background_pixel = 0; //XWhitePixel(xen::impl::unix_display, 0);
				frame_attributes.border_pixel = 0;
				////////////////////////////////////////////////////////////////////////


				////////////////////////////////////////////////////////////////////////
				// Create the X Window
				result->xwindow = XCreateWindow(result->display,         // open locally
				                                root,                    // parent window
				                                0, 0,                    // top left coords
				                                size.x, size.y,
				                                0,                       // border width
				                                color_depth,
				                                InputOutput,             // window type
				                                visual,
				                                CWBackPixel |            // Attribute mask
				                                CWColormap |
				                                CWBorderPixel,
				                                &frame_attributes        // Attributes
				                               );
				XSync(result->display, True);

				if(result->xwindow){
					// :TODO: log
					printf("INFO: Created x window %lu\n", result->xwindow);
				} else {
					// :TODO: log
					printf("ERROR: Failed to create x window\n");
					return nullptr;
				}
				////////////////////////////////////////////////////////////////////////


				////////////////////////////////////////////////////////////////////////
				// Determine the XVisualInfo that is in use (contains data about
				// color channels, etc)
				/*int num_visual_info      = 0;
				XVisualInfo visual_info_template;
				visual_info_template.visualid = XVisualIDFromVisual(visual);

				XVisualInfo* visual_info = XGetVisualInfo(xen::impl::unix_display,
				                                          VisualIDMask,
				                                          &visual_info_template,
				                                          &num_visual_info
				                                         );
				if(visual_info == nullptr){
					// :TODO: log
					printf("ERROR: Failed to determine visual info for window!\n");
					return nullptr;
				}

				// :TODO: log
				if(visual_info->c_class == TrueColor){
					printf("INFO: Using TrueColor visual\n");
				} else if (visual_info->c_class == DirectColor){
					printf("INFO: Using DirectColor visual\n");
				} else {
					printf("ERROR: Using unsupported visual type!\n");
					return nullptr;
				}
				if(visual_info->bits_per_rgb != 8){
					printf("ERROR: Expected 8 bits per color channel\n");
					return nullptr;
				}

				switch(visual_info->red_mask) {
				case 0xff000000: result->shift_r = 24; break;
				case 0x00ff0000: result->shift_r = 16; break;
				case 0x0000ff00: result->shift_r =  8; break;
				case 0x000000ff: result->shift_r =  0; break;
				default:
					printf("ERROR: Invalid red mask in visual info\n");
					return nullptr;
				}
				switch(visual_info->green_mask) {
				case 0xff000000: result->shift_g = 24; break;
				case 0x00ff0000: result->shift_g = 16; break;
				case 0x0000ff00: result->shift_g =  8; break;
				case 0x000000ff: result->shift_g =  0; break;
				default:
					printf("ERROR: Invalid green mask in visual info\n");
					return nullptr;
				}
				switch(visual_info->blue_mask) {
				case 0xff000000: result->shift_b = 24; break;
				case 0x00ff0000: result->shift_b = 16; break;
				case 0x0000ff00: result->shift_b =  8; break;
				case 0x000000ff: result->shift_b =  0; break;
				default:
					printf("ERROR: Invalid blue mask in visual info\n");
					return nullptr;
				}

				result->visual_info = *visual_info;

				XFree(visual_info);*/
			  ////////////////////////////////////////////////////////////////////////

				result->visual      = visual;

				////////////////////////////////////////////////////////////////////////
				// Setup what input events we want to capture for the window
				XSelectInput(result->display, result->xwindow,
				             //ExposureMask        | // Window shown
				             StructureNotifyMask | // Resize request, window moved
				             ResizeRedirectMask  | // Window resized
				             ButtonPressMask     | // Mouse
				             ButtonReleaseMask   |
				             KeyPressMask        | // Keyboard
				             KeyReleaseMask      |
				             0);

				// Change the close button behaviour so that we can capture event,
				// rather than the window manager destroying the window by itself
				Atom wm_delete_window = XInternAtom(result->display, "WM_DELETE_WINDOW", False);
				XSetWMProtocols(result->display, result->xwindow, &wm_delete_window, 1);
				////////////////////////////////////////////////////////////////////////

				////////////////////////////////////////////////////////////////////////
				// Show the window on screen
				setWindowTitle(result, title); // Set proper window title
				XMapWindow(result->display, result->xwindow);
				XMapRaised(result->display, result->xwindow);
				result->state |= xen::Window::IS_OPEN;
				////////////////////////////////////////////////////////////////////////

				// :TODO: don't really want to have to wait here, but need to do it
				// before we can draw
				// https://tronche.com/gui/x/xlib-tutorial/
				while(true){
					XEvent e;
					XNextEvent(result->display, &e);
					if(e.type == MapNotify){ break; }
				}

				transaction.commit();
				return result;
			}

		void destroyWindow(xen::Window* window){
			XDestroyWindow(window->display, window->xwindow);
			XCloseDisplay(window->display);
			*window = {};
		}
	} //end of namespace xen::impl::

	/// \brief Retrieves the size of the client area (ie, part that may be renderered on) of some window
	Vec2u getClientAreaSize(Window* window){
		XWindowAttributes attributes;
		XGetWindowAttributes(window->display, window->xwindow, &attributes);

    return { (u32)attributes.width, (u32)attributes.height };
	}

	bool isKeyPressed(Key key, xen::Window* window){
		KeySym  keysym  = xenKeysymFromKey(key);
		KeyCode keycode = XKeysymToKeycode(window->display, keysym);
		if(keycode == 0){ return false; }

		// Get state of all keys - each key corrosponds to a single bit
		char keys[32];
		XQueryKeymap(window->display, keys);

		// Check the keycode in question
		return (keys[keycode / 8] & (1 << (keycode % 8))) != 0;
	}

	void setWindowTitle(Window* window, const char* title){
		XStoreName(window->display, window->xwindow, title);
	}
}

#endif
