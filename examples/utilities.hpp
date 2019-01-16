////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Misc utility functions to be used by the xeno-engine examples
///
/// \ingroup examples
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLES_UTILITIES_HPP
#define XEN_EXAMPLES_UTILITIES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/graphics/Window.hpp>
#include <xen/graphics/GraphicsModuleApi.hpp>
#include <xen/kernel/Kernel.hpp>
#include <xen/kernel/Module.hpp>
#include <xen/kernel/threads.hpp>
#include <xen/kernel/log.hpp>

namespace xen {
	struct Window;
	struct Camera3dCylinder;
	struct Camera3d;
}

/////////////////////////////////////////////////////////////////////
/// \brief Handles the control input for a cylinder orbit camera
/////////////////////////////////////////////////////////////////////
void handleCameraInputCylinder(xen::Window* win, xen::Camera3dCylinder& camera, real dt, real max_radius = 750_r);

/////////////////////////////////////////////////////////////////////
/// \brief Handles the control input for a camera which moves around in
/// the xy plane
/////////////////////////////////////////////////////////////////////
void handleCameraInputPlane   (xen::Window* win, xen::Camera3d& camera, real dt);

#endif