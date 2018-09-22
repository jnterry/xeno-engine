////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declarations of helper functions common to all example
/// applications
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLES_COMMON_HPP
#define XEN_EXAMPLES_COMMON_HPP

#include <xen/sren/PostProcessor.hpp>

#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Window.hpp>
#include <xen/graphics/TestMeshes.hpp>
#include <xen/graphics/Light3d.hpp>

#include <xen/math/utilities.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/vertex_group_types.hpp>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/time.hpp>
#include <xen/core/array.hpp>

#include <stdio.h>

/////////////////////////////////////////////////////////////////////
/// \brief Holds variables required for running xeno engine
/// example applications
/////////////////////////////////////////////////////////////////////
struct ExampleApplication {
	xen::Allocator*      allocator;
	xen::ArenaLinear     arena;
	xen::GraphicsDevice* device;
	xen::Window*         window;

	enum Backend {
		RASTERIZER = 0,
		RAYTRACER,
		OPENGL,
		RAYTRACER_CAMERA_DEBUG,
		ATOMTRACER,
		ATOMTRACER_DEBUG,

		COUNT,
	};

	static const char* BACKEND_NAMES[Backend::COUNT];
};
static_assert(ExampleApplication::Backend::COUNT ==
              XenArrayLength(ExampleApplication::BACKEND_NAMES),
              "Expected number of backends to equal number of backend names"
             );


/////////////////////////////////////////////////////////////////////
/// \brief Creates an ExampleApplication. Shows the user a dialog to
/// pick rendering backend
/// \param window_title The title for the window to create
/// \param default_backend The type of rendering backend which will be
/// used if the user does not pick one
////////////////////////////////////////////////////////////////////
ExampleApplication createApplication(const char* window_title,
                                     ExampleApplication::Backend default_backend =
                                     ExampleApplication::Backend::COUNT,
                                     xen::Array<xsr::PostProcessor*> post_processors =
                                     xen::Array<xsr::PostProcessor*>::EmptyArray
                                    );

/////////////////////////////////////////////////////////////////////
/// \brief Destroys a previous created ExampleApplication
/////////////////////////////////////////////////////////////////////
void destroyApplication(ExampleApplication& app);

/////////////////////////////////////////////////////////////////////
/// \brief Handles the control input for a cylinder orbit camera
/////////////////////////////////////////////////////////////////////
void handleCameraInputCylinder(xen::Window* win, xen::Camera3dCylinder& camera, real dt, real max_radius = 750_r);

/////////////////////////////////////////////////////////////////////
/// \brief Handles the control input for a camera which moves around in
/// the xy plane
/////////////////////////////////////////////////////////////////////
void handleCameraInputPlane   (xen::Window* win, xen::Camera3d& camera, real dt);

/// \brief Utility for measuring fps
struct FpsCounter {
	/// \brief Internal timer to count time since last fps print
	xen::Stopwatch timer;

	/// \brief Counter for number of frames rendered since timer was last reset
	u32            counter;

	/// \brief Creates a new FpsCounter
	FpsCounter();

	/// \brief Increments the counter, prints the fps to stdout
	/// every half a second
	void update();
};

#endif
