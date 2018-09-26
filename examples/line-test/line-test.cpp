#include "../utilities.hpp"

#include <xen/graphics/TestMeshes.hpp>

#define LINE_COUNT_SQRT (50)
#define LINE_COUNT      (LINE_COUNT_SQRT * LINE_COUNT_SQRT)

struct State {
	xen::FixedArray<xen::VertexAttribute::Type, 2> vertex_spec;
	xen::Mesh mesh_axes;
	xen::Mesh mesh_parallel_lines;

	xen::Window* window;

	xen::RenderParameters3d render_params;
	xen::FixedArray<xen::RenderCommand3d, 3> render_commands;
};

State* state = nullptr;

void initRenderCommands(){
	xen::clearToZero(state->render_commands);

	state->render_commands[0].primitive_type         = xen::PrimitiveType::LINES;
	state->render_commands[0].color                  = xen::Color::WHITE4f;
	state->render_commands[0].model_matrix           = xen::Scale3d(100_r);
	state->render_commands[0].mesh                   = state->mesh_axes;

	state->render_commands[1].primitive_type         = xen::PrimitiveType::LINES;
	state->render_commands[1].color                  = xen::Color::YELLOW4f;
	state->render_commands[1].model_matrix           = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                                    xen::Scale3d(50_r, 50_r, 10_r) *
	                                                    xen::Rotation3dy( 30_deg)
	                                                   );
	state->render_commands[1].mesh                   = state->mesh_parallel_lines;

	state->render_commands[2].primitive_type         = xen::PrimitiveType::LINES;
	state->render_commands[2].color                  = xen::Color::MAGENTA4f;
	state->render_commands[2].model_matrix           = (xen::Translation3d(-0.5_r, -0.5_r, -0.5_r) *
	                                                    xen::Scale3d(50_r, 50_r, 10_r) *
	                                                    xen::Rotation3dy(-30_deg)
	                                                   );
	state->render_commands[2].mesh                   = state->mesh_parallel_lines;
}

void initMeshes(xen::GraphicsModuleApi* gmod){
	state->vertex_spec[0] = xen::VertexAttribute::Position3r;
	state->vertex_spec[1] = xen::VertexAttribute::Color4b;

	printf("create axes mesh\n");
	state->mesh_axes = gmod->createMesh(state->vertex_spec, xen::TestMeshGeometry_Axes);

	Vec3r parallel_lines_pbuf[LINE_COUNT * 2];

	printf("About to set line verts...\n");
	for(int xi = 0; xi < LINE_COUNT_SQRT; ++xi){
		real x = (real)xi / (real)LINE_COUNT_SQRT;
		for(int yi = 0; yi < LINE_COUNT_SQRT; ++yi){
			real y = (real)yi / (real)LINE_COUNT_SQRT;

			parallel_lines_pbuf[(xi*LINE_COUNT_SQRT + yi) * 2 + 0] = {x, y, 0};
			parallel_lines_pbuf[(xi*LINE_COUNT_SQRT + yi) * 2 + 1] = {x, y, 1};
		}
	}
	printf("Uploading verts...\n");
	state->mesh_parallel_lines = gmod->createMesh(state->vertex_spec,
	                                              XenArrayLength(parallel_lines_pbuf),
	                                              parallel_lines_pbuf,
	                                              nullptr
	                                             );
}

void* init( const void* params){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr,
	          "Expected graphics module to be loaded before line-test");

  state = (State*)xen::kernelAlloc(sizeof(State));

	state->window = gmod->createWindow({800, 600}, "line-test");

	state->render_params.camera.z_near   =  0.001;
	state->render_params.camera.z_far    =  1000;
	state->render_params.camera.fov_y    =  70_deg;
	state->render_params.camera.up_dir   =  Vec3r::UnitY;
	state->render_params.camera.look_dir = -Vec3r::UnitZ;
	state->render_params.camera.position =  Vec3r{0, 0, 50};

	state->render_params.ambient_light = {1,1,1};

	initMeshes(gmod);
	initRenderCommands();

	return state;
}

void* load( void* data, const void* params){
	state = (State*)state;
	return (void*)true;
}

void tick( const xen::TickContext& cntx){
	xen::GraphicsModuleApi* gmod = (xen::GraphicsModuleApi*)xen::getModuleApi("graphics");
	XenAssert(gmod != nullptr,
	          "Expected graphics module to be loaded before line-test");


	xen::Aabb2u viewport = { Vec2u::Origin, xen::getClientAreaSize(state->window) };

	xen::WindowEvent* event;
	while((event = xen::pollEvent(state->window)) != nullptr){
		switch(event->type){
		case xen::WindowEvent::Closed:
		  gmod->destroyWindow(state->window);
			break;
		default: break;
		}
	}
	handleCameraInputPlane(state->window, state->render_params.camera, xen::asSeconds<real>(cntx.dt));

	// Rendering
	gmod->clear      (state->window, xen::Color{20, 20, 20, 255});
	gmod->render     (state->window, viewport, state->render_params, state->render_commands);
	gmod->swapBuffers(state->window);

}

void shutdown(void* data, const void* params){
	xen::kernelFree(state);
}

xen::Module exported_xen_module = {
	xen::hash("game"),
	&init, &shutdown,
	&load, nullptr,
	&tick
};
