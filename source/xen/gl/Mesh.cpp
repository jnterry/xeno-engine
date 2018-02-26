////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API independent implementation of Mesh
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MESH_CPP
#define XEN_GL_MESH_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/util/File.hpp>
#include <xen/math/vector.hpp>
#include <xen/graphics/VertexAttribute.hpp>

#include "Mesh.hxx"
#include "gl_header.hxx"

#ifdef  XEN_USE_DOUBLE_PRECISION
#define TINYOBJLOADER_USE_DOUBLE
#endif
#define TINYOBJ_LOADER_C_IMPLEMENTATION
#include <tinyobj_loader_c.h>

namespace{
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

	xen::gl::MeshHeader* pushMeshHeader(xen::ArenaLinear& arena, u32 attrib_count){
		xen::MemoryTransaction transaction(arena);

		xen::gl::MeshHeader* result = xen::reserveType<xen::gl::MeshHeader>(arena);
		result->attribute_count     = attrib_count;
		result->attribute_types     = xen::reserveTypeArray<xen::VertexAttribute::Type    >(arena, attrib_count);
		result->attribute_sources   = xen::reserveTypeArray<xen::gl::VertexAttributeSource>(arena, attrib_count);

		if(xen::isValid(arena)){
			transaction.commit();
			return result;
		} else {
			return nullptr;
		}
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Gets the default source for a vertex attribute of the specified
	/// type. This will be a constant with some sensible value dependent on
	/// the vertex attribute type's aspect
	/////////////////////////////////////////////////////////////////////
	xen::gl::VertexAttributeSource getDefaultVertexAttributeSource(xen::VertexAttribute::Type type) {
		xen::gl::VertexAttributeSource source = {0};

		switch(type){
		case xen::VertexAttribute::Position3r:
			source.vec3r = {0,0,0};
			break;
		case xen::VertexAttribute::Normal3r:
			source.vec3r = {1,0,0};
			break;
		case xen::VertexAttribute::Color3f:
			source.color3f = xen::Color::WHITE4f.rgb;
			break;
		default:
			XenInvalidCodePath("Unhandled vertex attribute type while setting default source");
			break;
		}

		return source;
	}
}


xen::gl::MeshHeader* xen::gl::loadMesh(xen::ArenaLinear& arena, const char* path, u32 flags){
	//:TODO: this function is a mess (directly taken from tinyobj example code)
	// - we should take params of what VertexAttribs we want to load, and in what order in final mesh
	//   - currently loads color, but obj doesn't support colors, should replace with a constant value
	// - we need to support the flags

	xen::AllocatorMalloc alloc; //:TODO: dont really want to be creating allocator here...
	xen::ArenaLinear scratch = xen::createArenaLinear(alloc, xen::megabytes(4));

	xen::FileData file = xen::loadFileAndNullTerminate(scratch, path);
	if(file.size == 0){
		printf("Failed to open mesh file: %s\n", path);
		exit(1);
	}

	tinyobj_attrib_t attrib;
	tinyobj_shape_t* shapes = NULL;
	size_t num_shapes;
	tinyobj_material_t* materials = NULL;
	size_t num_materials;

	unsigned int obj_flags = TINYOBJ_FLAG_TRIANGULATE;
	int ret = tinyobj_parse_obj(&attrib,
	                            &shapes, &num_shapes,
	                            &materials, &num_materials,
	                            (char*)file.data, file.size,
	                            obj_flags
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

	xen::MemoryTransaction transaction(arena);
	xen::gl::MeshHeader* result = pushMeshHeader(arena, 3);

	result->attribute_types[0] = xen::VertexAttribute::Position3r;
	result->attribute_types[1] = xen::VertexAttribute::Color3f;
	result->attribute_types[2] = xen::VertexAttribute::Normal3r;

	static_assert(sizeof(float) == sizeof(real), "This code assumes that real is a float");
	result->attribute_sources[0].offset = 0 * sizeof(float);
	result->attribute_sources[1].offset = 6 * sizeof(float);
	result->attribute_sources[2].offset = 3 * sizeof(float);
	result->attribute_sources[0].stride = 9 * sizeof(float);
	result->attribute_sources[1].stride = 9 * sizeof(float);
	result->attribute_sources[2].stride = 9 * sizeof(float);

	for (size_t i = 0; i < attrib.num_face_num_verts; i++) {
		XenAssert(attrib.face_num_verts[i] == 3, "Assuming mesh triangulated");
		for (size_t f = 0; f < (size_t)attrib.face_num_verts[i] / 3; f++) {
			size_t k;
			float v[3][3];
			float n[3][3];
			float c[3];

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

				// :TODO:COMP: have method that can update aabb bounds with a new point
				result->bounds.min.elements[k] = (v[0][k] < result->bounds.min.elements[k]) ? v[0][k] : result->bounds.min.elements[k];
				result->bounds.min.elements[k] = (v[1][k] < result->bounds.min.elements[k]) ? v[1][k] : result->bounds.min.elements[k];
				result->bounds.min.elements[k] = (v[2][k] < result->bounds.min.elements[k]) ? v[2][k] : result->bounds.min.elements[k];
				result->bounds.max.elements[k] = (v[0][k] > result->bounds.max.elements[k]) ? v[0][k] : result->bounds.max.elements[k];
				result->bounds.max.elements[k] = (v[1][k] > result->bounds.max.elements[k]) ? v[1][k] : result->bounds.max.elements[k];
				result->bounds.max.elements[k] = (v[2][k] > result->bounds.max.elements[k]) ? v[2][k] : result->bounds.max.elements[k];
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

	GLuint buffer_handle = 0;

	if (num_triangles > 0) {
		glGenBuffers(1, &buffer_handle);
		for(u32 i = 0; i < 3; ++i){
			result->attribute_sources[i].buffer = buffer_handle;
		}
		glBindBuffer(GL_ARRAY_BUFFER, buffer_handle);
		glBufferData(GL_ARRAY_BUFFER, num_triangles * 3 * stride * sizeof(float), vb, GL_STATIC_DRAW);
		result->num_triangles = (int)num_triangles;
	}

	free(vb);
	free(scratch.start);
	tinyobj_attrib_free(&attrib);
	tinyobj_shapes_free(shapes, num_shapes);
	tinyobj_materials_free(materials, num_materials);

	printf("Successfully loaded mesh '%s', gpu_buf: %i, num faces: %i, bounds:(%f, %f, %f) -> (%f, %f, %f)\n",
	       path,
	       buffer_handle,
	       result->num_triangles,
	       result->bounds.min.x, result->bounds.min.y, result->bounds.min.z,
	       result->bounds.max.x, result->bounds.max.y, result->bounds.max.z
	       );

	transaction.commit();
	return result;
}

xen::gl::MeshHeader* xen::gl::createMesh(xen::ArenaLinear&                 arena,
                                         u08                               attrib_count,
                                         const xen::VertexAttribute::Type* attrib_types,
                                         const void**                      attrib_data,
                                         u32                               vertex_count,
                                         u32 flags){

	XenAssert(vertex_count % 3 == 0, "Mesh must be created from collection of triangles");

	xen::MemoryTransaction transaction(arena);
	xen::gl::MeshHeader* result = pushMeshHeader(arena, attrib_count);

	result->num_triangles = vertex_count / 3;

	u32 gpu_buffer_size =   0;
	u08 position_index  = 255; // index of attrib representing position

	///////////////////////////////////////////////
	// Create the GPU buffer
	GLuint gpu_buffer;
	XEN_CHECK_GL(glGenBuffers(1, &gpu_buffer));
	XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, gpu_buffer));

	///////////////////////////////////////////////
	// Set up mesh attributes, work out where to store data in gpu buffer
	for(u08 i = 0; i < attrib_count; ++i){
		result->attribute_types[i] = attrib_types[i];

		if((xen::VertexAttribute::_AspectPosition & attrib_types[i]) ==
		   xen::VertexAttribute::_AspectPosition){
			XenAssert(attrib_data[i] != nullptr, "Mesh's position data cannot be inferred");
			XenAssert(position_index == 255, "Mesh can only have single position attribute");
			position_index = i;
		}

		if(attrib_data[i] == nullptr){
			if((xen::VertexAttribute::_AspectNormal & attrib_types[i]) ==
			   xen::VertexAttribute::_AspectNormal) {
				// then normals will be computed down the line...
			} else {
				result->attribute_sources[i] = getDefaultVertexAttributeSource(attrib_types[i]);
				continue;
			}
		}

		result->attribute_sources[i].buffer = gpu_buffer;
		result->attribute_sources[i].offset = gpu_buffer_size;
		result->attribute_sources[i].stride = getVertexAttributeSize(attrib_types[i]);
		gpu_buffer_size += result->attribute_sources[i].stride * vertex_count;
	}

	///////////////////////////////////////////////
	// Calculate Mesh Bounds
	XenAssert(position_index != 255, "Mesh's position data cannot be inferred");
	Vec3r* positions = (Vec3r*)attrib_data[position_index];
	result->bounds.min = positions[0];
	result->bounds.max = positions[0];
	for(u32 i = 1; i < vertex_count; ++i){
		result->bounds.min = xen::min(result->bounds.min, positions[i]);
		result->bounds.max = xen::max(result->bounds.max, positions[i]);
	}

	///////////////////////////////////////////////
	// Reserve space for data
	XEN_CHECK_GL(glBufferData(GL_ARRAY_BUFFER, gpu_buffer_size, nullptr, GL_STATIC_DRAW));

	printf("Created mesh, gpu_buf: %i, num faces: %i, bounds:(%f, %f, %f) -> (%f, %f, %f)\n",
	       gpu_buffer,
	       result->num_triangles,
	       result->bounds.min.x, result->bounds.min.y, result->bounds.min.z,
	       result->bounds.max.x, result->bounds.max.y, result->bounds.max.z
	      );

	///////////////////////////////////////////////
	// Upload vertex attrib data to GPU buffer
	for(u08 i = 0; i < attrib_count; ++i){
		if(attrib_data[i] == nullptr &&
		   (
		    (result->attribute_types[i] & xen::VertexAttribute::_AspectNormal) !=
		    xen::VertexAttribute::_AspectNormal
		   )
		  ){
			// Then its a constant attribute, there is nothing to buffer
			continue;
		}

		result->attribute_sources[i].buffer = gpu_buffer;

		const void* data_source = attrib_data[i];
		bool  generated_data = false;

		// check if we need to generate normals
		if(((xen::VertexAttribute::_AspectNormal & result->attribute_types[i]) ==
		    xen::VertexAttribute::_AspectNormal
		   ) &&
		   data_source == nullptr){
			// Then generate normals
			generated_data = true;

			//:TODO: dont use malloc, use arena (or better have a thread local scratch space)
			Vec3r* normals = (Vec3r*)malloc(sizeof(Vec3r) * vertex_count);
			data_source = normals;
			for(u32 v = 0; v < vertex_count; v += 3){
				Vec3r n;
				CalcNormal(n.elements,
				           positions[v + 0].elements,
				           positions[v + 1].elements,
				           positions[v + 2].elements
				           );
				normals[v + 0] = n;
				normals[v + 1] = n;
				normals[v + 2] = n;
			}
		}

		XEN_CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER,
		                             result->attribute_sources[i].offset,
		                             getVertexAttributeSize(result->attribute_types[i]) * vertex_count,
		                             data_source
		                             )
		             );

		if(generated_data){ free((void*)data_source); }
	}

	///////////////////////////////////////////////
	// Return Result
	transaction.commit();
	return result;
}

#endif
