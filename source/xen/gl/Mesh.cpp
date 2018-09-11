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
#include <xen/core/File.hpp>
#include <xen/math/vector.hpp>
#include <xen/graphics/Mesh.hpp>

#include "Mesh.hxx"
#include "gl_header.hxx"

namespace{
	xen::gl::MeshGlData* pushMeshGlData(xen::ArenaLinear& arena, u32 attrib_count){
		xen::MemoryTransaction transaction(arena);

		xen::gl::MeshGlData* result = xen::reserveType<xen::gl::MeshGlData>(arena);

		if(!xen::isValid(arena)){ return nullptr; }

		result->vertex_spec.size     = attrib_count;
		result->vertex_spec.elements = xen::reserveTypeArray<xen::VertexAttribute::Type    >(arena, attrib_count);
		result->vertex_data = xen::reserveTypeArray<xen::gl::VertexAttributeSource>(arena, attrib_count);

		transaction.commit();
		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Gets the default source for a vertex attribute of the specified
	/// type. This will be a constant with some sensible value dependent on
	/// the vertex attribute type's aspect
	/////////////////////////////////////////////////////////////////////
	xen::gl::VertexAttributeSource getDefaultVertexAttributeSource(xen::VertexAttribute::Type type) {
		xen::gl::VertexAttributeSource source;
		xen::clearToZero(&source, sizeof(source));

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
		case xen::VertexAttribute::Color4b:
			source.color4b = xen::Color::WHITE;
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

xen::gl::MeshGlData* xen::gl::createMesh(xen::ArenaLinear& arena, const xen::MeshData& md){
	xen::MemoryTransaction transaction(arena);

	xen::gl::MeshGlData* result = pushMeshGlData(arena, md.vertex_spec.size);
	result->vertex_count = md.vertex_count;

	u32 gpu_buffer_size =   0;
	u08 position_index  = 255; // index of attrib representing position

	///////////////////////////////////////////////
	// Create the GPU buffer
	GLuint gpu_buffer;
	XEN_CHECK_GL(glGenBuffers(1, &gpu_buffer));
	XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, gpu_buffer));

	///////////////////////////////////////////////
	// Set up mesh attributes, work out where to store data in gpu buffer
	for(u08 i = 0; i < md.vertex_spec.size; ++i){
		result->vertex_spec[i] = md.vertex_spec[i];

		if((xen::VertexAttribute::_AspectPosition ==
		    (md.vertex_spec[i] & xen::VertexAttribute::_AspectMask))){
			XenAssert(position_index == 255, "Mesh can only have single position attribute");
			XenAssert(md.vertex_data[i] != nullptr, "Mesh's position data cannot be inferred");
			position_index = i;
		}

		if(md.vertex_data[i] == nullptr){
			result->vertex_data[i] = getDefaultVertexAttributeSource(md.vertex_spec[i]);
			continue;
		}

		result->vertex_data[i].buffer = gpu_buffer;
		result->vertex_data[i].offset = gpu_buffer_size;
		result->vertex_data[i].stride = getVertexAttributeSize(md.vertex_spec[i]);
		gpu_buffer_size += result->vertex_data[i].stride * md.vertex_count;
	}

	///////////////////////////////////////////////
	// Calculate Mesh Bounds
	XenAssert(position_index != 255,
	          "Mesh's does not contain position attribute - and it cannot be inferred");
	const Vec3r* positions = (const Vec3r*)md.vertex_data[position_index];
	result->bounds.min = positions[0];
	result->bounds.max = positions[0];
	for(u32 i = 1; i < md.vertex_count; ++i){
		result->bounds.min = xen::min(result->bounds.min, positions[i]);
		result->bounds.max = xen::max(result->bounds.max, positions[i]);
	}

	///////////////////////////////////////////////
	// reserve space
	XEN_CHECK_GL(glBufferData(GL_ARRAY_BUFFER, gpu_buffer_size, nullptr, GL_STATIC_DRAW));

	printf("Created mesh, gpu_buf: %i, num verts: %i, bounds:(%f, %f, %f) -> (%f, %f, %f)\n",
	       gpu_buffer,
	       result->vertex_count,
	       result->bounds.min.x, result->bounds.min.y, result->bounds.min.z,
	       result->bounds.max.x, result->bounds.max.y, result->bounds.max.z
	      );

	///////////////////////////////////////////////
	// Upload vertex attrib data to GPU buffer
	for(u08 i = 0; i < md.vertex_spec.size; ++i){
		if(md.vertex_data[i] == nullptr) {
			// Then its a constant attribute, there is nothing to buffer
			continue;
		}

		result->vertex_data[i].buffer = gpu_buffer;

		const void* data_source = md.vertex_data[i];

		XEN_CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER,
		                             result->vertex_data[i].offset,
		                             getVertexAttributeSize(result->vertex_spec[i]) * md.vertex_count,
		                             data_source
		                             )
		            );
	}

	///////////////////////////////////////////////
	// Return Result
	transaction.commit();
	return result;
}

void xen::gl::updateMeshAttribData(xen::gl::MeshGlData* mesh,
                                   u32                  attrib_index,
                                   void*                new_data,
                                   u32                  start_vertex,
                                   u32                  end_vertex
                                   ){

	end_vertex = xen::min(end_vertex, mesh->vertex_count);
	if(end_vertex < start_vertex){ return; }

	VertexAttributeSource* attrib_source = &mesh->vertex_data[attrib_index];

	u32 attrib_size = xen::getVertexAttributeSize(mesh->vertex_spec[attrib_index]);

	if(attrib_source->buffer == 0){

		// Then no existing data for this attribute, so create a new one

		if(start_vertex != 0 || end_vertex != mesh->vertex_count){
			// :TODO: log
			printf("WARN: Updating mesh attrib %i but there is no existing data and "
			       "the new data set is not complete\n", attrib_index);
		}

		XEN_CHECK_GL(glGenBuffers(1, &attrib_source->buffer));
		XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, attrib_source->buffer));

		// :TODO: here we create the buffer as dynamic draw
		// If a buffer already exists as created by createMesh it will be
		// GL_STATIC_DRAW. Do we want to allow the user to hint which attributes
		// are likely to be changed so segregate dynamic data into its own buffer?

		XEN_CHECK_GL(glBufferData(GL_ARRAY_BUFFER, attrib_size * mesh->vertex_count,
		                          nullptr, GL_DYNAMIC_DRAW));
	} else {
		XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, attrib_source->buffer));
	}


	XEN_CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER,
	                             attrib_source->offset + (start_vertex * attrib_size),
	                             (end_vertex - start_vertex) * attrib_size,
	                             new_data
	                            )
	            );
}

#endif
