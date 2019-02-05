////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of utility functions used by example applications
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_EXAMPLE_UTILITES_CPP
#define XEN_EXAMPLE_UTILITES_CPP

#include "utilities.hpp"

#include <xen/graphics/Camera3d.hpp>
#include <xen/window/Window.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/utilities.hpp>

xen::MaterialParameterSource phong_parameter_sources[] = {
	{ "mvp_mat",                 xen::MaterialParameterSource::MvpMatrix             },
	{ "model_mat",               xen::MaterialParameterSource::ModelMatrix           },
	{ "ambient_light",           xen::MaterialParameterSource::AmbientLightColor     },
	{ "camera_position",         xen::MaterialParameterSource::CameraWorldPosition   },
	{ "point_light_pos",         xen::MaterialParameterSource::PointLightPosition    },
	{ "point_light_color",       xen::MaterialParameterSource::PointLightColor       },
	{ "point_light_attenuation", xen::MaterialParameterSource::PointLightAttenuation },
	{ "diffuse_map",             xen::MaterialParameterSource::TextureChannel0       },
};
const char* phong_vertex_files[] = {
	"resource/material/phong_vertex.glsl"
};
const char* phong_pixel_files[]  = {
	"resource/material/phong_pixel.glsl", "resource/material/lighting.glsl"
};

xen::MaterialCreationParameters material_creation_params_phong = {
	{ XenArrayLength(phong_vertex_files     ), phong_vertex_files },
	{ 0, nullptr },
	{ XenArrayLength(phong_pixel_files      ), phong_pixel_files  },
	{ XenArrayLength(phong_parameter_sources), phong_parameter_sources },
};

////////////////////////////////////////////////////////////////////////////////

xen::MaterialParameterSource normal_lines_parameter_sources[] = {
	{ "mvp_mat",                 xen::MaterialParameterSource::MvpMatrix             },
	{ "model_mat",               xen::MaterialParameterSource::ModelMatrix           },
};
const char* normal_lines_vertex_files[] = {
	"resource/material/normal_lines_vertex.glsl",
};
const char* normal_lines_geometry_files[]  = {
	"resource/material/normal_lines_geometry.glsl",
};
const char* normal_lines_pixel_files[]  = {
	"resource/material/normal_lines_pixel.glsl",
};

xen::MaterialCreationParameters material_creation_params_normal_lines = {
	{ XenArrayLength(normal_lines_vertex_files     ), normal_lines_vertex_files   },
  { XenArrayLength(normal_lines_geometry_files   ), normal_lines_geometry_files },
	{ XenArrayLength(normal_lines_pixel_files      ), normal_lines_pixel_files    },
	{ XenArrayLength(normal_lines_parameter_sources), normal_lines_parameter_sources },
};

////////////////////////////////////////////////////////////////////////////////

void handleCameraInputCylinder(xen::ModuleApiWindow* mod_win, xen::Window* win, xen::Camera3dCylinder& camera, real dt, real max_radius){
	if(!mod_win->hasFocus(win)){ return; }

	// compute speed such that can get from full zoom to no zoom in 3 seconds
	const real       camera_speed        = (max_radius / 3_r);
	const xen::Angle camera_rotate_speed = 120_deg;

	if(mod_win->isKeyPressed(xen::Key::ArrowUp)){
		camera.radius -= camera_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::ArrowDown)){
		camera.radius += camera_speed * dt;
	}
	camera.radius = xen::clamp(camera.radius, 0.005_r, max_radius);

	if(mod_win->isKeyPressed(xen::Key::ArrowLeft)){
		camera.angle -= camera_rotate_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::ArrowRight)){
		camera.angle += camera_rotate_speed * dt;
	}

	if(mod_win->isKeyPressed(xen::Key::A)){
		camera.height += camera_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::Z)){
		camera.height -= camera_speed * dt;
	}

	if(mod_win->isKeyPressed(xen::Key::Q)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
	if(mod_win->isKeyPressed(xen::Key::E)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

void handleCameraInputPlane(xen::ModuleApiWindow* mod_win, xen::Window* win, xen::Camera3d& camera, real dt) {
	if(!mod_win->hasFocus(win)){ return; }

	const static constexpr real camera_speed = 1.0f;

	if(mod_win->isKeyPressed(xen::Key::ArrowUp)){
		camera.position.y += camera_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::ArrowDown)){
	  camera.position.y -= camera_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::ArrowLeft)){
		camera.position.x += camera_speed * dt;
	}
	if(mod_win->isKeyPressed(xen::Key::ArrowRight)){
		camera.position.x -= camera_speed * dt;
	}

	if(mod_win->isKeyPressed(xen::Key::A)){
		camera.position.z -= camera_speed * dt;
		camera.position.z = xen::max(0.001_r, camera.position.z);
	}
	if(mod_win->isKeyPressed(xen::Key::Z)){
		camera.position.z += camera_speed * dt;
	}

	if(mod_win->isKeyPressed(xen::Key::Q)){
		camera.up_dir = xen::rotated(camera.up_dir,  Vec3r::UnitZ, 90_deg * dt);
	}
	if(mod_win->isKeyPressed(xen::Key::E)){
		camera.up_dir = xen::rotated(camera.up_dir, -Vec3r::UnitZ, 90_deg * dt);
	}
}

#endif
