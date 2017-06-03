#include <stdio.h>

#include <SFML/Window/Window.hpp>
#include <SFML/Window/Event.hpp>

#include <GL/glew.h>
#include <GL/gl.h>

#include "xen/core/intrinsics.hpp"
#include "xen/core/memory.hpp"
#include "xen/graphics/Shader.hpp"
#include "xen/math/Vector.hpp"
#include "xen/math/Matrix.hpp"
#include "xen/math/Angle.hpp"
#include "xen/math/Quaternion.hpp"

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
void renderCube();
void renderMesh(const Mesh& mesh);
xen::ShaderProgram* loadShader(xen::ArenaLinear&);
Mesh loadMesh(const char* path);


Camera3dOrbit camera = {0};
real camera_speed = 10;
xen::Angle camera_rotate_speed = 120_deg;
xen::Angle camera_pitch = 0_deg;


int main(int argc, char** argv){
	camera.z_near   = 0.001;
	camera.z_far    = 10000;
	camera.fov_y    = 80_deg;
	camera.radius   = 10;
	camera.angle    = 90_deg;

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

	Mat4r view_mat;

	xen::ShaderProgram* prog = loadShader(arena);
	Mesh mesh_bunny = loadMesh("bunny.obj");
	int mvpMatLoc = xen::getUniformLocation(prog, "mvpMatrix");
	Mat4r model_mat;

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
			default: break;
			}
		}

		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Up)){
			camera.radius -= camera_speed * dt;
		}
		if(sf::Keyboard::isKeyPressed(sf::Keyboard::Down)){
			camera.radius += camera_speed * dt;
		}
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

		view_mat = getViewMatrix(camera, window_size);

		app.setActive(true);
		glClearColor(0.2,0.2,0.2, 1);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		xen::useShader(prog);

		model_mat  = Mat4r::Identity;
		model_mat *= xen::Rotation3dx(time * 41_deg);
		model_mat *= xen::Rotation3dy(time * 67_deg);
		model_mat *= xen::Rotation3dz(time * 83_deg);
		model_mat *= xen::Scale3d(0.3 + sin(time*15)*0.03, 0.3 + sin(time*15 + 0.25*xen::PI)*0.03, 0.3 + sin(time*15 + 0.5*xen::PI)*0.03);

		model_mat *= xen::Translation3d(xen::rotated(Vec3r{7, 0, 0},
		                                             Vec3r::UnitY, xen::Degrees(time*90_r)));
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		renderCube();

		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(30);
		model_mat *= xen::Translation3d(0, 0, 0);
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		renderMesh(mesh_bunny);

		model_mat = Mat4r::Identity;
		model_mat *= xen::Scale3d(5, 0.05, 5);
		model_mat *= xen::Translation3d(0, -3, 0);
		xen::setUniform(mvpMatLoc, model_mat * view_mat);
		renderCube();

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
    0.0f, 1.0f, 0.0f
};

void initCube(){
	glGenBuffers(1, &cube_vert_buffer);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glBufferData(GL_ARRAY_BUFFER, sizeof(cube_buffer_data),
	             cube_buffer_data, GL_STATIC_DRAW);
}


void renderCube(){
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glBindBuffer(GL_ARRAY_BUFFER, cube_vert_buffer);
	glVertexAttribPointer(0,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_FALSE, // normalized
	                      0,        // stride
	                      (void*)0  // start offset
	                      );
	glVertexAttribPointer(1,        // attrib layout
	                      3,        // components
	                      GL_FLOAT, // type
	                      GL_TRUE,  // normalized
	                      0,        // stride
	                      (void*)(sizeof(float)*3*12*3)  // start offset
	                      );
	glDrawArrays(GL_TRIANGLES, 0, 12*3);
}

void renderMesh(const Mesh& mesh){
	glEnableVertexAttribArray(0);

	glBindBuffer(GL_ARRAY_BUFFER, mesh.gpu_buffer);
	glVertexAttribPointer(0,                                 // attrib layout
	                      3, GL_FLOAT,                       // components and type
	                      GL_FALSE,                          // normalized
	                      sizeof(float)*9,                   // stride
	                      (const void*)0                     // offset
	                      );
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

	FileData vertex_src = loadFileAndNullTerminate(scratch, "vertex.glsl");
	FileData pixel_src  = loadFileAndNullTerminate(scratch, "pixel.glsl");

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

	FileData file = loadFileAndNullTerminate(arena, path);
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
				} else { /* normal index is not defined for this face */
					/* compute geometric normal */
					CalcNormal(n[0], v[0], v[1], v[2]);
					n[1][0] = n[0][0];
					n[1][1] = n[0][1];
					n[1][2] = n[0][2];
					n[2][0] = n[0][0];
					n[2][1] = n[0][1];
					n[2][2] = n[0][2];
				}
			} else {
				/* compute geometric normal */
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

				/* Use normal as color. */
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

				vb[(3 * i + k) * stride + 6] = (c[0] * 0.5f + 0.5f);
				vb[(3 * i + k) * stride + 7] = (c[1] * 0.5f + 0.5f);
				vb[(3 * i + k) * stride + 8] = (c[2] * 0.5f + 0.5f);
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
