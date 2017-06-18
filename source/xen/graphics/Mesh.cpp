////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Mesh.cpp
/// \author Jamie Terry
/// \date 2017/06/17
/// \brief Contains render API independent implementation of Mesh
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_CPP
#define XEN_GRAPHICS_MESH_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory.hpp>
#include <xen/util/File.hpp>
#include <xen/math/Vector.hpp>
#include <xen/graphics/Mesh.hpp>

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

	u32 getAttribTypeSize(xen::VertexAttrib::Type type){
		switch(type){
		case xen::VertexAttrib::PositionXYZ: return sizeof(Vec3r);
		case xen::VertexAttrib::NormalXYZ:   return sizeof(Vec3r);
		case xen::VertexAttrib::ColorRGBf:   return sizeof(Vec3f);
		}
		// compiler should warn about for missing case above to prevent this
		XenInvalidCodePath();
		return 0;
	}

	xen::Mesh* pushMesh(xen::ArenaLinear& arena, u32 attrib_count){
		xen::Mesh* result = (xen::Mesh*)xen::ptrGetAdvanced(arena.next_byte, sizeof(xen::GpuBuffer));
		result->gpu_buffer = (xen::GpuBuffer*)arena.next_byte;
		xen::ptrAdvance(&arena.next_byte,
		                sizeof(xen::GpuBuffer) + sizeof(xen::Mesh) + sizeof(xen::VertexAttrib)*attrib_count);

		result->attrib_count = attrib_count;

		return result;
	}

}

namespace xen{
	xen::Mesh* loadMesh(xen::ArenaLinear& arena, const char* path, u32 flags){
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
		xen::Mesh* result = pushMesh(arena, 3);

		result->attrib_count = 3;
		result->attribs[0].type = xen::VertexAttrib::PositionXYZ;
		result->attribs[1].type = xen::VertexAttrib::ColorRGBf;
		result->attribs[2].type = xen::VertexAttrib::NormalXYZ;
		result->attribs[0].offset = 0 * sizeof(float);
		result->attribs[1].offset = 6 * sizeof(float);
		result->attribs[2].offset = 3 * sizeof(float);
		result->attribs[0].stride = 9 * sizeof(float);
		result->attribs[1].stride = 9 * sizeof(float);
		result->attribs[2].stride = 9 * sizeof(float);

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
					result->bounds_min.elements[k] = (v[0][k] < result->bounds_min.elements[k]) ? v[0][k] : result->bounds_min.elements[k];
					result->bounds_min.elements[k] = (v[1][k] < result->bounds_min.elements[k]) ? v[1][k] : result->bounds_min.elements[k];
					result->bounds_min.elements[k] = (v[2][k] < result->bounds_min.elements[k]) ? v[2][k] : result->bounds_min.elements[k];
					result->bounds_max.elements[k] = (v[0][k] > result->bounds_max.elements[k]) ? v[0][k] : result->bounds_max.elements[k];
					result->bounds_max.elements[k] = (v[1][k] > result->bounds_max.elements[k]) ? v[1][k] : result->bounds_max.elements[k];
					result->bounds_max.elements[k] = (v[2][k] > result->bounds_max.elements[k]) ? v[2][k] : result->bounds_max.elements[k];
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

		result->gpu_buffer->handle = 0;
		result->num_triangles      = 0;

		if (num_triangles > 0) {
			glGenBuffers(1, &result->gpu_buffer->handle);
			glBindBuffer(GL_ARRAY_BUFFER, result->gpu_buffer->handle);
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
		       result->gpu_buffer->handle,
		       result->num_triangles,
		       result->bounds_min.x, result->bounds_min.y, result->bounds_min.z,
		       result->bounds_max.x, result->bounds_max.y, result->bounds_max.z
		       );

		transaction.commit();
		return result;
	}

	Mesh* createMesh(ArenaLinear& arena,
	                 u08 attrib_count, VertexAttrib::Type* attrib_types,
	                 u32 vertex_count, const void** attrib_data,
	                 u32 flags){

		XenAssert(vertex_count % 3 == 0, "Mesh must be created from collection of triangles");

		xen::MemoryTransaction transaction(arena);
		xen::Mesh* result = pushMesh(arena, attrib_count);

		result->num_triangles = vertex_count / 3;

		u32 gpu_buffer_size =   0;
		u08 position_index  = 255; // index of attrib representing position

		///////////////////////////////////////////////
		// Set up mesh attributes, work out where to store data in gpu buffer
		for(u08 i = 0; i < attrib_count; ++i){
			result->attribs[i].type = attrib_types[i];
			result->attribs[i].offset = gpu_buffer_size;

			switch(attrib_types[i]){
			case VertexAttrib::PositionXYZ:
				XenAssert(attrib_data[i] != nullptr, "Mesh's position data cannot be inferred");
				XenAssert(position_index == 255, "Mesh can only have single position attribute");
				result->attribs[i].stride = sizeof(Vec3r);
				position_index = i;
				break;
			case VertexAttrib::NormalXYZ:
				result->attribs[i].stride = sizeof(Vec3r);
				break;
			case VertexAttrib::ColorRGBf:
				if(attrib_data[i] == nullptr){
					result->attribs[i].stride = 0;
					result->attribs[i].value3f = Vec3f(1,1,1); // white by default
				} else {
					result->attribs[i].stride = sizeof(Vec3f);
				}
				break;
			}

			gpu_buffer_size += result->attribs[i].stride * vertex_count;

		}

		///////////////////////////////////////////////
		// Calculate Mesh Bounds
		XenAssert(position_index != 255, "Mesh's position data cannot be inferred");
		Vec3r* positions = (Vec3r*)attrib_data[position_index];
		result->bounds_min = positions[0];
		result->bounds_max = positions[0];
		for(u32 i = 1; i < vertex_count; ++i){
			result->bounds_min.x = XenMin(result->bounds_min.x, positions[i].x);
			result->bounds_min.y = XenMin(result->bounds_min.y, positions[i].y);
			result->bounds_min.z = XenMin(result->bounds_min.z, positions[i].z);
			result->bounds_max.x = XenMax(result->bounds_max.x, positions[i].x);
			result->bounds_max.y = XenMax(result->bounds_max.y, positions[i].y);
			result->bounds_max.z = XenMax(result->bounds_max.z, positions[i].z);
		}

	    ///////////////////////////////////////////////
		// Create the GPU buffer
		XEN_CHECK_GL(glGenBuffers(1, &result->gpu_buffer->handle));
		XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, result->gpu_buffer->handle));

		///////////////////////////////////////////////
		// Reserve space for data
		XEN_CHECK_GL(glBufferData(GL_ARRAY_BUFFER, gpu_buffer_size, nullptr, GL_STATIC_DRAW));

		///////////////////////////////////////////////
		// Upload vertex attrib data to GPU buffer
		for(u08 i = 0; i < attrib_count; ++i){
			if(result->attribs[i].stride){
				const void* data_source = attrib_data[i];
				bool  generated_data = false;

				// check if we need to generate normals
				if(result->attribs[i].type == VertexAttrib::NormalXYZ && data_source == nullptr){
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
				                             result->attribs[i].offset,
				                             getAttribTypeSize(result->attribs[i].type) * vertex_count,
				                             data_source
				                             )
				             );

				if(generated_data){ free((void*)data_source); }
			}
		}

		///////////////////////////////////////////////
		// Return Result
		transaction.commit();
		return result;
	}

}

#endif
