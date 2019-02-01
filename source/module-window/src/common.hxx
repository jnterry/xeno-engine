////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of functions whose implementations is provided
/// by common platform independent code
///
/// \ingroup module-window
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULEWINDOW_COMMON_HXX
#define XEN_MODULEWINDOW_COMMON_HXX

namespace xen {
	struct WindowEvent;
	struct Window;
	struct ModuleApiWindow;
}

namespace xwn {
	xen::WindowEvent* pollEvent(xen::Window* win);

	/////////////////////////////////////////////////////////////////////
	/// \brief Pushes some event onto the event queue. Makes a copy of
	/// the specified event, thus caller retains ownership of the event instance
	/// If the queue is already full then overwrites the oldest event and
	/// returns false.
	/////////////////////////////////////////////////////////////////////
	bool pushEvent(xen::Window* win, const xen::WindowEvent& event);

	bool isWindowOpen(const xen::Window* win);
	bool hasFocus(const xen::Window* win);

	void initApiFunctionPointers(xen::ModuleApiWindow* api);
}

#endif
