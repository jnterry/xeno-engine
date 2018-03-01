////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API agnositc implementation of GraphicsDevice
/// related functions
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSDEVICE_CPP
#define XEN_GRAPHICS_GRAPHICSDEVICE_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/graphics/GraphicsDevice.hpp>

#include "Window.hxx"

namespace {
	/// \brief Map from device id to a pointer to the device
	xen::GraphicsDevice* created_devices[256] = {0};

	/// \brief The id of the next device that will be created
	u08 next_device_id = 0;
}

namespace xen {
	GraphicsDevice::GraphicsDevice()
		: id(next_device_id) {

		// :TODO: next_device_id should be incremented atomically user tries
		// creating a device per thread

		XenAssert(next_device_id < 255,
		          "Maximum number of graphics devices have already been created"
		         );

		++next_device_id;

		created_devices[this->id] = this;
	}

	GraphicsDevice::~GraphicsDevice(){
		created_devices[this->id] = nullptr;
	}

	GraphicsDevice* getGraphicsDevice(u08 id){
		return created_devices[id];
	}

	void GraphicsDevice::clear(Window* window,
	           const xen::Aabb2u& viewport,
	           xen::Color color
	          ){
		clear(window->render_target, viewport, color);
	}

	void GraphicsDevice::render(Window* window,
	            const xen::Aabb2u& viewport,
	            const RenderParameters3d& params,
	            const xen::Array<RenderCommand3d> commands
	            ){
		render(window->render_target, viewport, params, commands);
	}

	Window* GraphicsDevice::createWindow(Vec2u size, const char* title){
		// :TODO: remove this -> this just makes it compile since not virutal void
		// until we define in all derived classes
		XenBreak();
		return nullptr;
	}
	void GraphicsDevice::destroyWindow(Window* window){
		XenBreak();
	}
	void GraphicsDevice::swapBuffers(Window* window){
		XenBreak();
	}
}

#endif
