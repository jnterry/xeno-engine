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
#include <xen/graphics/Mesh.hpp>

#include "Mesh.hxx"
#include "gl_header.hxx"

namespace{
	xen::gl::MeshHeader* pushMeshHeader(xen::ArenaLinear& arena, u32 attrib_count){
		xen::MemoryTransaction transaction(arena);

		xen::gl::MeshHeader* result = xen::reserveType<xen::gl::MeshHeader>(arena);

		if(!xen::isValid(arena)){ return nullptr; }

		result->attribute_count     = attrib_count;
		result->attribute_types     = xen::reserveTypeArray<xen::VertexAttribute::Type    >(arena, attrib_count);
		result->attribute_sources   = xen::reserveTypeArray<xen::gl::VertexAttributeSource>(arena, attrib_count);

		// :TODO:COMP: helper function -> return transaction.commitIfValid(arena, result);
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
		xen::gl::VertexAttributeSource source;
		xen::clearBytes(&source, sizeof(source));

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
		case xen::VertexAttribute::TexCoord2f:
			source.vec2f   = {0, 0 };
			break;
		default:
			XenInvalidCodePath("Unhandled vertex attribute type while setting default source");
			break;
		}

		return source;
	}
}

xen::gl::MeshHeader* xen::gl::createMesh(xen::ArenaLinear& arena, const xen::MeshData& md){
	XenAssert(md.vertex_count % 3 == 0, "Mesh must be created from collection of triangles");

	xen::MemoryTransaction transaction(arena);
	xen::gl::MeshHeader* result = pushMeshHeader(arena, md.attrib_count);

	result->num_triangles = md.vertex_count / 3;

	u32 gpu_buffer_size =   0;
	u08 position_index  = 255; // index of attrib representing position

	///////////////////////////////////////////////
	// Create the GPU buffer
	GLuint gpu_buffer;
	XEN_CHECK_GL(glGenBuffers(1, &gpu_buffer));
	XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, gpu_buffer));

	///////////////////////////////////////////////
	// Set up mesh attributes, work out where to store data in gpu buffer
	for(u08 i = 0; i < md.attrib_count; ++i){
		result->attribute_types[i] = md.attrib_types[i];

		if((xen::VertexAttribute::_AspectPosition ^ (md.attrib_types[i] & xen::VertexAttribute::_AspectMask)) == 0){
			XenAssert(position_index == 255, "Mesh can only have single position attribute");
			XenAssert(md.attrib_data[i] != nullptr, "Mesh's position data cannot be inferred");
			position_index = i;
		}

		if(md.attrib_data[i] == nullptr){
			result->attribute_sources[i] = getDefaultVertexAttributeSource(md.attrib_types[i]);
			continue;
		}

		result->attribute_sources[i].buffer = gpu_buffer;
		result->attribute_sources[i].offset = gpu_buffer_size;
		result->attribute_sources[i].stride = getVertexAttributeSize(md.attrib_types[i]);
		gpu_buffer_size += result->attribute_sources[i].stride * md.vertex_count;
	}

	///////////////////////////////////////////////
	// Calculate Mesh Bounds
	XenAssert(position_index != 255,
	          "Mesh's does not contain position attribute - and it cannot be inferred");
	const Vec3r* positions = (const Vec3r*)md.attrib_data[position_index];
	result->bounds.min = positions[0];
	result->bounds.max = positions[0];
	for(u32 i = 1; i < md.vertex_count; ++i){
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
	for(u08 i = 0; i < md.attrib_count; ++i){
		if(md.attrib_data[i] == nullptr) {
			// Then its a constant attribute, there is nothing to buffer
			continue;
		}

		result->attribute_sources[i].buffer = gpu_buffer;

		const void* data_source = md.attrib_data[i];

		XEN_CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER,
		                             result->attribute_sources[i].offset,
		                             getVertexAttributeSize(result->attribute_types[i]) * md.vertex_count,
		                             data_source
		                             )
		            );
	}

	///////////////////////////////////////////////
	// Return Result
	transaction.commit();
	return result;
}

#endif
