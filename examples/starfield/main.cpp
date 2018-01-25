#include <stdio.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/core/random.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Shader.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/Texture.hpp>
#include <xen/graphics/gl_header.hxx>
#include <xen/graphics/Camera3d.hpp>
#include <xen/math/utilities.hpp>
#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Angle.hpp>
#include <xen/math/Quaternion.hpp>

#include "SDLauxilary.h"

xen::Camera3dOrbit camera;
real camera_speed = 10;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

const u32 STAR_COUNT = 1024;

Vec3r star_positions[STAR_COUNT];

int main(int argc, char** argv){
	camera.z_near   = 0.1;
	camera.z_far    = 100;
	camera.fov_y    = 1_deg;
	camera.radius   = 100;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	//:TODO: breaks if angle is exactly +90deg, never occurs
	// under user control since don't hit dead on float value, but
	// broken if set here
	camera.angle    = -90.0_deg;

	Vec2r window_size = {800, 600};
	screen* screen = InitializeSDL(window_size.x, window_size.y, false);

	int grid_count = xen::sqrt(STAR_COUNT);
	for(u32 i = 0; i < STAR_COUNT; ++i){
		//int grid_x = i % grid_count;
		//int grid_y = i / grid_count;
		//star_positions[i].x = xen::lerp(-100, 100, (float)grid_x / (float)grid_count);
		//star_positions[i].y = xen::lerp(-100, 100, (float)grid_y / (float)grid_count);
		//star_positions[i].z = -100;

		star_positions[i].x = xen::randf(-100, 100);
		star_positions[i].y = xen::randf(-100, 100);
		star_positions[i].z = xen::randf(-100, 100);
	}

	int last_tick = SDL_GetTicks();

	printf("Entering main loop\n");
	while(NoQuitMessageSDL()) {
		int tick = SDL_GetTicks();
		float dt = ((float)(tick - last_tick)) / 1000.0f;
		last_tick = tick;

		for(u32 i = 0; i < STAR_COUNT; ++i){
			star_positions[i].z += dt * 50.0f;
			if(star_positions[i].z >= 100.0f){
				star_positions[i].z = -100.0f;
			}
		}

		// Clear buffer
		memset(screen->buffer, 0, screen->height*screen->width*sizeof(uint32_t));

		//camera.angle += dt * 30_deg;
		Mat4f mat_vp = xen::getViewProjectionMatrix(camera, window_size);

		Vec3f screen_space;
		xen::Color color = {255, 255, 255, 0};

		for(u32 i=0; i < STAR_COUNT; ++i) {

			Vec3f clip_space = star_positions[i] * mat_vp;

			/*if(clip_space.x < -1 || clip_space.x > 1 ||
			   clip_space.y < -1 || clip_space.y > 1 ||
			   clip_space.z < -1 || clip_space.z > 1){
				printf("%f, %f, %f\n", clip_space.x, clip_space.y, clip_space.z);
				continue;
				}*/

			Vec2f screen_space = clip_space.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * window_size;

			PutPixelSDL(screen, screen_space.x, screen_space.y, color);
		}

		for(int axis = 0; axis < 3; ++axis){
			xen::Color color;
			color.value = 0xFF << 8 * (axis);
			for(float delta = 0; delta < 100; delta += 0.1f){
				Vec3f world_space  = Vec3r::Origin;
				world_space.elements[axis] = delta;
				Vec3f clip_space = world_space * mat_vp;
				Vec2f screen_space = clip_space.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * window_size;

				for(int dx = -2; dx <= 2; ++dx){
					for(int dy = -2; dy <= 2; ++dy){
						PutPixelSDL(screen, screen_space.x, screen_space.y, color);
					}
				}
			}
		}

		for(int axis = 0; axis < 3; ++axis){
			xen::Color color;
			color.value = 0xFF << 8 * (axis);
			for(float delta = 0; delta < 100; delta += 0.1f){
				Vec3f world_space  = Vec3r::Origin;
				world_space.elements[axis] = delta;
				Vec3f clip_space = world_space * mat_vp;
				Vec2f screen_space = clip_space.xy + (((Vec2f){1.0f, 1.0f}) / 2.0f) * window_size;

				for(int dx = -2; dx <= 2; ++dx){
					for(int dy = -2; dy <= 2; ++dy){
						PutPixelSDL(screen, screen_space.x, screen_space.y, color);
					}
				}
			}
		}



		SDL_Renderframe(screen);
	}
	printf("Exiting main loop\n");

	SDL_SaveImage(screen, "screenshot.bmp");
	KillSDL(screen);

	return 0;
}
