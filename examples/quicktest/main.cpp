#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Shader.hpp>
#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Angle.hpp>
#include <xen/math/Quaternion.hpp>

#include "utilities.hpp"
#include "Camera3d.hpp"

#define TINYOBJ_LOADER_C_IMPLEMENTATION
#include "tinyobj_loader_c.h"


struct Mesh{
	GLuint gpu_buffer;
	u32   num_triangles;
	Vec3r bounds_min;
	Vec3r bounds_max;
};
void initCube();
void renderCube(Vec3r color);
void renderMesh(const Mesh& mesh);
xen::ShaderProgram* loadShader(xen::ArenaLinear&);
Mesh loadMesh(const char* path);


Camera3dOrbit camera;
real camera_speed = 10;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;

int main(int argc, char** argv){
	camera.z_near   = 0.001;
	camera.z_far    = 10000;
	camera.fov_y    = 80_deg;
	camera.radius   = 10;
	camera.height   = 0;
	camera.up_dir   = Vec3r::UnitY;
	//:TODO: breaks if angle is exactly +90deg, never occurs
	// under user control since dont hit dead on float value, but
	// broken if set here
	camera.angle    = -90_deg;

	sf::ContextSettings context_settings;
	context_settings.depthBits = 24;
	context_settings.stencilBits = 8;
	context_settings.antialiasingLevel = 4;
	context_settings.majorVersion = 3;
	context_settings.minorVersion = 0;

	Vec2r window_size = {800, 600};

	sf::Window app(sf::VideoMode(window_size.x, window_size.y, 32), "Window Title", sf::Style::Default, context_settings);

	context_settings = app.getSettings();
	printf("Initialized window, GL version: %i.%i\n", context_settings.majorVersion, context_settings.minorVersion);

	XenTempArena(arena, 4096);

	app.setActive(true);
	glewInit();
	initCube();

	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);

	Mat4r model_mat, view_mat, proj_mat, vp_mat;
	Vec4r point_light_color = Vec4r(1,0,0,1);

	xen::ShaderProgram* prog = loadShader(arena);
	Mesh mesh_bunny = loadMesh("bunny.obj");
	int mvp_mat_loc           = xen::getUniformLocation(prog, "mvp_mat"          );
	int model_mat_loc         = xen::getUniformLocation(prog, "model_mat"        );
	int point_light_pos_loc   = xen::getUniformLocation(prog, "point_light_pos"  );
	int point_light_color_loc = xen::getUniformLocation(prog, "point_light_color");
	int emissive_color_loc    = xen::getUniformLocation(prog, "emissive_color"   );
	int camera_pos_loc        = xen::getUniformLocation(prog, "camera_position"  );

	sf::Clock timer;
	real last_time = 0;

	Vec3r ctest = xen::cross(Vec3r{1,2,4}, Vec3r{9,8,7});
	printf("Cross: %f, %f, %f\n", ctest.x, ctest.y ,ctest.z);

	printf("Entering main loop\n");
	while(app.isOpen()){
		float time = timer.getElapsedTime().asSeconds();
		real dt = time - last_time;
		last_time = time;

		sf::Event event;
		while(app.pollEvent(event)){
			switch(event.type){
			case sf::Event::Closed:
				app.close();
				break;
			case sf::Event::Resized:
				glViewport(0,0,event.size.width, event.size.height);
				window_size = {(real)event.size.width, (real)event.size.height};
				break;
			case sf::Event::KeyReleased:
				switch(event.key.code){
				case sf::Keyboard::R:
					point_light_color.xyz = Vec3r(1,0,0);
					break;
				case sf::Keyboard::G:
					point_light_color.xyz = Vec3r(0,1,0);
					break;
				case sf::Keyboard::B:
					point_light_color.xyz = Vec3r(0,0,1);
					break;
				case sf::Keyboard::W:
					point_light_color.xyz = Vec3r(1,1,1);
					break;
				default: break;
				}
				break;
			default: break;
			}
		}

		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Up)){
			camera.radius -= camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Down)){
			camera.radius += camera_speed * dt;
		}
		camera.radius = xen::clamp(camera.radius, 0.01_r, 100_r);
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Left)){
			camera.angle -= camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Right)){
			camera.angle += camera_rotate_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::A)){
			camera.height += camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Z)){
			camera.height -= camera_speed * dt;
		}

		view_mat = getViewMatrix(camera);
		proj_mat = getProjectionMatrix(camera, window_size);
		vp_mat   = view_mat * proj_mat;

		app.setActive(true);
		glClearColor(0.3,0.3,0.3, 1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		xen::useShader(prog);

		Vec3r light_pos = xen::rotated(Vec3r{4, 3, 0}, Vec3r::UnitY, xen::Degrees(time*90_r));
		xen::setUniform(point_light_pos_loc, light_pos);
		point_light_color.w = (1_r + sin(time*9)) / 2.0_r;
		xen::setUniform(point_light_color_loc, point_light_color);
		xen::setUniform(camera_pos_loc, getCameraPosition(camera));

		////////////////////////////////////////////
		// Draw Cube Light
		model_mat  = Mat4r::Identity;
		model_mat *= xen::Rotation3dx(time * 41_deg);
		model_mat *= xen::Rotation3dy(time * 67_deg);
		model_mat *= xen::Rotation3dz(time * 83_deg);
		model_mat *= xen::Scale3d(0.3 + sin(time*15)*0.03,
		                          0.3 + sin(time*15 + 0.25*xen::PI)*0.03,
		                          0.3 + sin(time*15 + 0.5*xen::PI)*0.03);
		model_mat *= xen::Translation3d(light_pos);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		xen::setUniform(emissive_color_loc, point_light_color);
		renderCube(point_light_color.xyz);
		xen::setUniform(emissive_color_loc, Vec4r::Origin);
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Bunny
		model_mat = Mat4r::Identity;
		//model_mat *= xen::Rotation3dz(73_deg * time);
		model_mat *= xen::Scale3d(20);
		model_mat *= xen::Translation3d(-0.5_r * (mesh_bunny.bounds_max - mesh_bunny.bounds_min));
		model_mat *= xen::Rotation3dy(67_deg * time);
		//model_mat *= xen::Translation3d(0, 0, 0);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		renderMesh(mesh_bunny);
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Floor
		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(30, 0.05, 30);
		model_mat *= xen::Translation3d(0, -0.5, 0);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		renderCube(Vec3r(0.7,0.7,0.7));
		////////////////////////////////////////////

		////////////////////////////////////////////
		// Draw Axes
		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(1,0,0);
		model_mat *= xen::Scale3d(5, 0.05, 0.05);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		xen::setUniform(emissive_color_loc, Vec4r(1,0,0,1));
		renderCube(Vec3r(Vec3r::UnitX));

		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(0,1,0);
		model_mat *= xen::Scale3d(0.05, 5, 0.05);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		xen::setUniform(emissive_color_loc, Vec4r(0,1,0,1));
		renderCube(Vec3r(Vec3r::UnitY));

		model_mat = Mat4r::Identity;
		model_mat *= xen::Translation3d(0,0,1);
		model_mat *= xen::Scale3d(0.05, 0.05, 5);
		xen::setUniform(mvp_mat_loc, model_mat * vp_mat);
		xen::setUniform(model_mat_loc, model_mat);
		xen::setUniform(emissive_color_loc, Vec4r(0,0,1,1));
		renderCube(Vec3r(Vec3r::UnitZ));
		////////////////////////////////////////////

		app.display();
	}
	printf("Exiting main loop\n");

	return 0;
}

GLuint cube_vert_buffer;

static const GLfloat cube_buffer_data[] = {
    -1.0f,-1.0f,-1.0f, // triangle 1 : begin
    -1.0f,-1.0f, 1.0f,
    -1.0f, 1.0f, 1.0f, // triangle 1 : end
     1.0f, 1.0f,-1.0f, // triangle 2 : begin
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f,-1.0f, // triangle 2 : end
     1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f,-1.0f,
     1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f, 1.0f,
    -1.0f,-1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
    -1.0f,-1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f,-1.0f,
     1.0f,-1.0f,-1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
     1.0f, 1.0f,-1.0f,
    -1.0f, 1.0f,-1.0f,
     1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f,-1.0f,
    -1.0f, 1.0f, 1.0f,
     1.0f, 1.0f, 1.0f,
    -1.0f, 1.0f, 1.0f,
     1.0f,-1.0f, 1.0f,


    // color
    0.0f, 0.0f, 0.0f, // Face A
    0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 1.0f, // Face B
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 0.0f, // Face C
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 1.0f, // Face B
    1.0f, 0.0f, 1.0f,
    1.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 0.0f, // Face A
    0.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f, // Face C
    1.0f, 0.0f, 0.0f,
    1.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, // Face E
    0.0f, 1.0f, 0.0f,
    0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 1.0f, // Face D
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f, // Face D
    0.0f, 0.0f, 1.0f,
    0.0f, 0.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // Face F
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f, // Face F
    1.0f, 1.0f, 1.0f,
    1.0f, 1.0f, 1.0f,
    0.0f, 1.0f, 0.0f, // Face E
    0.0f, 1.0f, 0.0f,
    0.0f, 1.0f, 0.0f,

    // normals
    -1.0f, 0.0f, 0.0f, // triangle 1 : begin
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f, // triangle 1 : end
     0.0f, 0.0f,-1.0f, // triangle 2 : begin
     0.0f, 0.0f,-1.0f,
     0.0f, 0.0f,-1.0f, // triangle 2 : end
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f,
    -1.0f, 0.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f,-1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     1.0f, 0.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 1.0f, 0.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f,
     0.0f, 0.0f, 1.0f
};

void initCube(){
	glGenBuffers(1, &cube_vert_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(cube_buffer_data),
	             cube_buffer_data, GL_STATIC_DRAW);
}


void renderCube(Vec3r color){
	glEnableVertexAttribArray(0);
	glDisableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glVertexAttribPointer(0,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_FALSE, // normalized
	                      0,        // stride
	                      (void*)0  // start offset
	                      );
	glVertexAttrib3f(1, color.x,color.y,color.z);
	//glVertexAttribPointer(1,        // attrib layout
	//                      3,        // components
	//                      GL_FLOAT, // type
	//                      GL_TRUE,  // normalized
	//                      0,        // stride
	//                      (void*)(sizeof(float)*3*12*3*1)  // start offset
	//                      );
	glVertexAttribPointer(2,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_TRUE,  // normalized
	                      0,        // stride
	                      (void*)(sizeof(float)*3*12*3*2)  // start offset
	                      );
	glDrawArrays(GL_TRIANGLES, 0, 12*3);
}

void renderMesh(const Mesh& mesh){
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);

	glBindBuffer(GL_ARRAY_BUFFER, mesh.gpu_buffer);
	// positions
	glVertexAttribPointer(0,                                 // attrib layout
	                      3, GL_FLOAT,                       // components and type
	                      GL_FALSE,                          // normalized
	                      sizeof(float)*9,                   // stride
	                      (const void*)0                     // offset
	                      );

	// color
	glVertexAttribPointer(2,                                 // attrib layout
	                      3, GL_FLOAT,                       // components and type
	                      GL_FALSE,                          // normalized
	                      sizeof(float)*9,                   // stride
	                      (const void*)(sizeof(float) * 3)   // offset
	                      );

	// normals
	glVertexAttribPointer(1,                                 // attrib layout
	                      3, GL_FLOAT,                       // components and type
	                      GL_FALSE,                          // normalized
	                      sizeof(float)*9,                   // stride
	                      (const void*)(sizeof(float) * 6)   // offset
	                      );

	glDrawArrays(GL_TRIANGLES, 0, mesh.num_triangles * 3);
}

xen::ShaderProgram* loadShader(xen::ArenaLinear& arena){
	XenTempArena(scratch, 8196);

	xen::FileData vertex_src = xen::loadFileAndNullTerminate(scratch, "vertex.glsl");
	xen::FileData pixel_src  = xen::loadFileAndNullTerminate(scratch, "pixel.glsl");

	auto result = xen::createShaderProgram(arena, (char*)vertex_src.data, (char*)pixel_src.data);

	if(!xen::isOkay(result)){
		xen::resetArena(scratch);
		const char* errors = xen::getErrors(result, scratch);
		printf("Shader Errors:\n%s\n", errors);
	} else {
		printf("Shader compiled successfully\n");
	}

	return result;
}

static void CalcNormal(float N[3], float v0[3], float v1[3], float v2[3]) {
  float v10[3];
  float v20[3];
  float len2;

  v10[0] = v1[0] - v0[0];
  v10[1] = v1[1] - v0[1];
  v10[2] = v1[2] - v0[2];

  v20[0] = v2[0] - v0[0];
  v20[1] = v2[1] - v0[1];
  v20[2] = v2[2] - v0[2];

  N[0] = v20[1] * v10[2] - v20[2] * v10[1];
  N[1] = v20[2] * v10[0] - v20[0] * v10[2];
  N[2] = v20[0] * v10[1] - v20[1] * v10[0];

  len2 = N[0] * N[0] + N[1] * N[1] + N[2] * N[2];
  if (len2 > 0.0f) {
    float len = (float)sqrt((double)len2);

    N[0] /= len;
    N[1] /= len;
  }
}

Mesh loadMesh(const char* path){
	xen::AllocatorMalloc alloc;

	xen::ArenaLinear arena = xen::createArenaLinear(alloc, xen::megabytes(4));

	xen::FileData file = xen::loadFileAndNullTerminate(arena, path);
	if(file.size == 0){
		printf("Failed to open mesh file: %s\n", path);
		exit(1);
	}

	tinyobj_attrib_t attrib;
	tinyobj_shape_t* shapes = NULL;
	size_t num_shapes;
	tinyobj_material_t* materials = NULL;
	size_t num_materials;

	unsigned int flags = TINYOBJ_FLAG_TRIANGULATE;
	int ret = tinyobj_parse_obj(&attrib,
	                            &shapes, &num_shapes,
	                            &materials, &num_materials,
	                            (char*)file.data, file.size,
	                            flags
	                            );

	if(ret != TINYOBJ_SUCCESS){
		printf("Failed to parse obj file: %s\n", path);
	}

	size_t face_offset = 0;

	// Assume triangulated faces
	size_t num_triangles = attrib.num_face_num_verts;
	size_t stride = 9; // 9 = pos(3float), normal(3float), color(3float)

	// allocate space for vert buffer data
	float* vb = (float*)malloc(sizeof(float) * stride * num_triangles * 3);

	Mesh result;

	for (size_t i = 0; i < attrib.num_face_num_verts; i++) {
		XenAssert(attrib.face_num_verts[i] == 3, "Assuming mesh triangulated");
		for (size_t f = 0; f < (size_t)attrib.face_num_verts[i] / 3; f++) {
			size_t k;
			float v[3][3];
			float n[3][3];
			float c[3];
			float len2;

			tinyobj_vertex_index_t idx0 = attrib.faces[face_offset + 3 * f + 0];
			tinyobj_vertex_index_t idx1 = attrib.faces[face_offset + 3 * f + 1];
			tinyobj_vertex_index_t idx2 = attrib.faces[face_offset + 3 * f + 2];

			for (k = 0; k < 3; k++) {
				int f0 = idx0.v_idx;
				int f1 = idx1.v_idx;
				int f2 = idx2.v_idx;
				XenAssert(f0 >= 0, "Index must be >= 0");
				XenAssert(f1 >= 0, "Index must be >= 0");
				XenAssert(f2 >= 0, "Index must be >= 0");

				v[0][k] = attrib.vertices[3 * (size_t)f0 + k];
				v[1][k] = attrib.vertices[3 * (size_t)f1 + k];
				v[2][k] = attrib.vertices[3 * (size_t)f2 + k];
				result.bounds_min.elements[k] = (v[0][k] < result.bounds_min.elements[k]) ? v[0][k] : result.bounds_min.elements[k];
				result.bounds_min.elements[k] = (v[1][k] < result.bounds_min.elements[k]) ? v[1][k] : result.bounds_min.elements[k];
				result.bounds_min.elements[k] = (v[2][k] < result.bounds_min.elements[k]) ? v[2][k] : result.bounds_min.elements[k];
				result.bounds_max.elements[k] = (v[0][k] > result.bounds_max.elements[k]) ? v[0][k] : result.bounds_max.elements[k];
				result.bounds_max.elements[k] = (v[1][k] > result.bounds_max.elements[k]) ? v[1][k] : result.bounds_max.elements[k];
				result.bounds_max.elements[k] = (v[2][k] > result.bounds_max.elements[k]) ? v[2][k] : result.bounds_max.elements[k];
			}

			if (attrib.num_normals > 0) {
				int f0 = idx0.vn_idx;
				int f1 = idx1.vn_idx;
				int f2 = idx2.vn_idx;
				if (f0 >= 0 && f1 >= 0 && f2 >= 0) {
					assert(f0 < (int)attrib.num_normals);
					assert(f1 < (int)attrib.num_normals);
					assert(f2 < (int)attrib.num_normals);
					for (k = 0; k < 3; k++) {
						n[0][k] = attrib.normals[3 * (size_t)f0 + k];
						n[1][k] = attrib.normals[3 * (size_t)f1 + k];
						n[2][k] = attrib.normals[3 * (size_t)f2 + k];
					}
				} else { // normal index is not defined for this face
					//compute geometric normal
					CalcNormal(n[0], v[0], v[1], v[2]);
					n[1][0] = n[0][0];
					n[1][1] = n[0][1];
					n[1][2] = n[0][2];
					n[2][0] = n[0][0];
					n[2][1] = n[0][1];
					n[2][2] = n[0][2];
				}
			} else {
				//compute geometric normal
				CalcNormal(n[0], v[0], v[1], v[2]);
				n[1][0] = n[0][0];
				n[1][1] = n[0][1];
				n[1][2] = n[0][2];
				n[2][0] = n[0][0];
				n[2][1] = n[0][1];
				n[2][2] = n[0][2];
			}

			for (k = 0; k < 3; k++) {
				vb[(3 * i + k) * stride + 0] = v[k][0];
				vb[(3 * i + k) * stride + 1] = v[k][1];
				vb[(3 * i + k) * stride + 2] = v[k][2];
				vb[(3 * i + k) * stride + 3] = n[k][0];
				vb[(3 * i + k) * stride + 4] = n[k][1];
				vb[(3 * i + k) * stride + 5] = n[k][2];

				// Use normal as color
				#if 0
				c[0] = n[k][0];
				c[1] = n[k][1];
				c[2] = n[k][2];
				len2 = c[0] * c[0] + c[1] * c[1] + c[2] * c[2];
				if (len2 > 0.0f) {
					float len = (float)sqrt((double)len2);

					c[0] /= len;
					c[1] /= len;
					c[2] /= len;
				}
				for(int i = 0; i < 3; ++ i){
					c[i] *= 0.5f;
					c[i] += 0.5f;
				}
				#else
				c[0] = 1;
				c[1] = 1;
				c[2] = 1;
				#endif
				vb[(3 * i + k) * stride + 6] = c[0];
				vb[(3 * i + k) * stride + 7] = c[1];
				vb[(3 * i + k) * stride + 8] = c[2];
			}
		}
		face_offset += (size_t)attrib.face_num_verts[i];
	}

	result.gpu_buffer    = 0;
	result.num_triangles = 0;

	if (num_triangles > 0) {
		glGenBuffers(1, &result.gpu_buffer);
		glBindBuffer(GL_ARRAY_BUFFER, result.gpu_buffer);
		glBufferData(GL_ARRAY_BUFFER, num_triangles * 3 * stride * sizeof(float), vb, GL_STATIC_DRAW);
		result.num_triangles = (int)num_triangles;
	}

	free(vb);
	free(arena.start);

	printf("Successfully loaded mesh '%s', num faces: %i, bounds:(%f, %f, %f) -> (%f, %f, %f)\n",
	       path, result.num_triangles,
	       result.bounds_min.x, result.bounds_min.y, result.bounds_min.z,
	       result.bounds_max.x, result.bounds_max.y, result.bounds_max.z
	       );
	return result;
}
