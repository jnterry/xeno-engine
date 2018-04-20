#include <stdio.h>

#include "../common.cpp"
#include <xen/graphics/Image.hpp>
#include <xen/sren/FragmentShader.hpp>

xen::Camera3dCylinder                  camera;
xen::RenderParameters3d                render_params;
xen::FixedArray<xen::LightSource3d, 1> scene_lights;

xen::FixedArray<xen::VertexAttribute::Type, 4> vertex_spec;

xen::Mesh    mesh_xzplane;

xen::Texture texture_bricks_diffuse;
xen::Texture texture_bricks_normal;
xen::Texture texture_metal_diffuse;
xen::Texture texture_metal_normal;

xen::Shader  shader_normal_map;

xen::FixedArray<xen::RenderCommand3d, 1> render_commands;

xen::Color4f FragmentShader_NormalMap(const xen::sren::FragmentUniforms& uniforms,
                                      Vec3r                              pos_world,
                                      Vec3r                              normal_world,
                                      xen::Color4f                       color,
                                      Vec2f                              uvs){
  xen::Color3f total_light = uniforms.ambient_light;
  total_light += (uniforms.emissive_color.rgb * uniforms.emissive_color.a);

  xen::Color4f normal_map = xen::sren::sampleTexture(uniforms.textures[1], uvs);
  Vec3r normal = xen::normalized(normal_world + normal_map.xyz);

  for(u32 i = 0; i < xen::size(uniforms.lights); ++i){
	  if(uniforms.lights[i].type != xen::LightSource3d::POINT){
		  printf("WARN: Unsupported light type in rasterizer\n");
		  continue;
	  }

	  real dist_sq_world = xen::distanceSq(pos_world, uniforms.lights[i].point.position);

	  total_light += xen::sren::computeLightInfluencePhong
		  ( uniforms.lights[i].point.position,
		    uniforms.lights[i].color,
		    uniforms.lights[i].attenuation,
		    dist_sq_world,
		    uniforms.camera.position,
		    pos_world, normal
		    );
  }

  for(u32 i = 0; i < 3; ++i){
	  if(total_light.elements[i] > 1){
		  total_light.elements[i] = 1;
	  }
  }

  xen::Color4f result = uniforms.diffuse_color;
	result     *= color;
	result     *= xen::sren::sampleTexture(uniforms.textures[0], uvs);
	result.rgb *= total_light;

  return result;
}

void initRenderCommands(){
	xen::clearToZero(render_commands);

	render_commands[0].primitive_type  = xen::PrimitiveType::TRIANGLES;
	render_commands[0].color           = xen::Color::WHITE4f;
	render_commands[0].model_matrix    = Mat4r::Identity;
	render_commands[0].mesh            = mesh_xzplane;
	render_commands[0].textures[0]     = texture_bricks_diffuse;
	render_commands[0].textures[1]     = texture_bricks_normal;
	render_commands[0].shader          = shader_normal_map;
}

void initCamera(){
	camera.z_near   = 0.001;
	camera.z_far    = 1000;
	camera.fov_y    = 70_deg;
	camera.radius   = 3;
	camera.height   = 1;
	camera.up_dir   = Vec3r::UnitY;
	camera.axis     = Vec3r::UnitY;
	camera.target   = Vec3r::Origin;
	camera.angle    = 0.0_deg;
}

void initSceneLights(){
	xen::clearToZero(&render_params, sizeof(xen::RenderParameters3d));

	scene_lights[0].type           = xen::LightSource3d::POINT;
	scene_lights[0].point.position = Vec3r{0.5_r, 0.5_r, 0.0_r};
	scene_lights[0].color          = Vec4f{1.0f, 0.95f, 0.8f, 0.5f};
	scene_lights[0].attenuation    = {0.0f, 0.0f, 2.0f};

	render_params.ambient_light = xen::Color3f(0.1f, 0.1f, 0.1f);
	render_params.lights        = scene_lights;
}

void initMeshes(xen::GraphicsDevice* device, xen::ArenaLinear& arena){
	xen::MemoryTransaction transaction(arena);

	vertex_spec[0] = xen::VertexAttribute::Position3r;
	vertex_spec[1] = xen::VertexAttribute::Normal3r;
	vertex_spec[2] = xen::VertexAttribute::Color4b;
	vertex_spec[3] = xen::VertexAttribute::TexCoord2f;

	mesh_xzplane = device->createMesh(vertex_spec,
	                                  xen::TestMeshGeometry_UnitXzPlaneCentered
	                                 );

	xen::RawImage image_bricks_diffuse = xen::loadImage(arena, "bricks-diffuse.jpg");
	xen::RawImage image_bricks_normal  = xen::loadImage(arena, "bricks-normal.jpg");
	xen::RawImage image_metal_diffuse  = xen::loadImage(arena, "metal-plate-diffuse.jpg");
	xen::RawImage image_metal_normal   = xen::loadImage(arena, "metal-plate-normal.jpg");

	texture_bricks_diffuse = device->createTexture(&image_bricks_diffuse);
	texture_bricks_normal  = device->createTexture(&image_bricks_normal);
	texture_metal_diffuse = device->createTexture(&image_metal_diffuse);
	texture_metal_normal  = device->createTexture(&image_metal_normal);

	shader_normal_map = device->createShader((void*)&FragmentShader_NormalMap);
}

int main(int argc, char** argv){
	initCamera();
	initSceneLights();

	ExampleApplication app = createApplication("torus",
	                                           ExampleApplication::Backend::RASTERIZER
	                                          );

	initMeshes(app.device, app.arena);
	initRenderCommands();

	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(app.window) };

	xen::Stopwatch timer;
	real last_time = 0;
	printf("Entering main loop\n");

	FpsCounter fps_counter;

	while(xen::isWindowOpen(app.window)) {
		real time = xen::asSeconds<real>(timer.getElapsedTime());
		real dt = time - last_time;
		last_time = time;

		xen::WindowEvent* event;
		while((event = xen::pollEvent(app.window)) != nullptr){
			switch(event->type){
			case xen::WindowEvent::Closed:
				app.device->destroyWindow(app.window);
				break;
			default: break;
			}
		}

		if(xen::isKeyPressed(xen::Key::Num1)){ // bricks
			render_commands[0].textures[0]     = texture_bricks_diffuse;
			render_commands[0].textures[1]     = texture_bricks_normal;
		}
		if(xen::isKeyPressed(xen::Key::Num2)){ // metal
		  render_commands[0].textures[0]     = texture_metal_diffuse;
			render_commands[0].textures[1]     = texture_metal_normal;
		}

		handleCameraInputCylinder(camera, dt, 30);
		render_params.camera = xen::generateCamera3d(camera);

		scene_lights[0].point.position = xen::rotated(Vec3r{0.5_r, 0.5_r, 0.0_r},
		                                              Vec3r::UnitY,
		                                              180_deg * time
		                                             );

		app.device->clear      (app.window, xen::Color{20, 20, 20, 255});
	  app.device->render     (app.window, viewport, render_params, render_commands);
	  app.device->swapBuffers(app.window);

	  fps_counter.update();
	}
	printf("Exiting main loop\n");

	destroyApplication(app);

	return 0;
}
