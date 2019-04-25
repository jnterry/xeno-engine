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
#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/kernel/log.hpp>

#include "Mesh.hxx"
#include "gl_header.hxx"
#include "ModuleGl.hxx"

namespace{
	/////////////////////////////////////////////////////////////////////
	/// \brief Gets the default source for a vertex attribute of the specified
	/// type. This will be a constant with some sensible value dependent on
	/// the vertex attribute type's aspect
	/////////////////////////////////////////////////////////////////////
	xgl::VertexAttributeSource getDefaultVertexAttributeSource(xen::VertexAttribute attrib) {
		xgl::VertexAttributeSource source;
		xen::clearToZero(&source, sizeof(source));

		u08 component_type  = attrib.type & xen::VertexAttribute::Type::ComponentTypeMask;

		switch(attrib.aspect){
		case xen::VertexAttribute::Aspect::Position:
		case xen::VertexAttribute::Aspect::TexCoord:
		case xen::VertexAttribute::Aspect::Custom:
			break; // already zero initialised
		case xen::VertexAttribute::Aspect::Normal: {
			if(component_type == xen::VertexAttribute::Type::Float){
				source.vec2f.x = 1.0f;
			} else if (component_type == xen::VertexAttribute::Type::Double) {
				source.vec2d.x = 1.0;
			} else {
					XenInvalidCodePath("Invalid type for normal data");
			}
			break;
		}
		case xen::VertexAttribute::Aspect::Color:
			if       (attrib.type == (xen::VertexAttribute::Type::Float | 3)){
				source.color3f = xen::Color::WHITE4f.rgb;
			} else if(attrib.type == (xen::VertexAttribute::Type::Byte  | 4)){
				source.color4b = xen::Color::WHITE;
			} else {
				XenInvalidCodePath("Invalid type for color data");
			}
			break;
		}
		return source;
	}
}

xgl::MeshGlData* doCreateMesh(const xen::VertexSpec& vertex_spec,
                              u16                    primitive_type,
                              bool*                  attrib_is_varying,
                              u32                    vertex_count,
                              GLenum                 buffer_usage){
	xgl::MeshGlData* result = xen::reserveType(xgl::state->pool_mesh);
	if(result == nullptr){
		XenLogError("Cannot create mesh as maximum number reached");
		return nullptr;
	}
	xen::clearToZero(result);

	xen::ArenaLinear& arena = xgl::state->primary_arena;
	xen::MemoryTransaction transaction(arena);

	result->vertex_spec.size     = vertex_spec.size;
	result->primitive_type       = primitive_type;
	result->vertex_spec.elements = xen::reserveTypeArray<xen::VertexAttribute>(arena, vertex_spec.size);
	result->vertex_data          = xen::reserveTypeArray<xgl::VertexAttributeSource>(arena, vertex_spec.size);
	result->vertex_count         = vertex_count;

	if(result->vertex_spec.elements == nullptr || result->vertex_data == nullptr){
		XenLogError("Cannot create mesh as out of memory to store CPU side meta data");
		xen::freeType(xgl::state->pool_mesh, result);
		return nullptr;
	}

	u32 gpu_buffer_size =   0;

	///////////////////////////////////////////////
	XenLogDebug("Creating mesh GPU buffer");
	GLuint gpu_buffer;
	XEN_CHECK_GL(glGenBuffers(1, &gpu_buffer));
	XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, gpu_buffer));

	///////////////////////////////////////////////
	XenLogDebug("Computing locations for mesh attribute data within GPU buffer");
	for(u08 i = 0; i < vertex_spec.size; ++i){
		result->vertex_spec[i] = vertex_spec[i];

		if(!attrib_is_varying[i]){
			result->vertex_data[i] = getDefaultVertexAttributeSource(vertex_spec[i]);
			continue;
		}

		result->vertex_data[i].buffer = gpu_buffer;
		result->vertex_data[i].offset = gpu_buffer_size;
		result->vertex_data[i].stride = getVertexAttributeSize(vertex_spec[i]);
		gpu_buffer_size += result->vertex_data[i].stride * vertex_count;
	}

	///////////////////////////////////////////////
	XenLogDebug("Reserving %u bytes of GPU memory for mesh attribute data", gpu_buffer_size);
	XEN_CHECK_GL(glBufferData(GL_ARRAY_BUFFER, gpu_buffer_size, nullptr, buffer_usage));

	transaction.commit();
	return result;
}

const xen::Mesh* xgl::createMesh(const xen::MeshData* md){

	// max of 255 attributes in a mesh
	bool attrib_is_varying[255] = {0};
	for(unsigned i = 0; i < md->vertex_spec.size; ++i){
		attrib_is_varying[i] = md->vertex_data[i] != nullptr;
	}

	xgl::MeshGlData* result = doCreateMesh(md->vertex_spec, md->primitive_type,
	                                       attrib_is_varying,
	                                       md->vertex_count, GL_STATIC_DRAW);

	///////////////////////////////////////////////
	XenLogDebug("Uploading mesh attribute data to GPU");
	for(u08 i = 0; i < md->vertex_spec.size; ++i){
		if(md->vertex_data[i] == nullptr) {
			// Then its a constant attribute, there is nothing to buffer
			continue;
		}

		const void* data_source = md->vertex_data[i];

		XGL_CHECK(glBufferSubData(
			          GL_ARRAY_BUFFER,
			          result->vertex_data[i].offset,
			          getVertexAttributeSize(result->vertex_spec[i]) * md->vertex_count,
			          data_source
		          ));
	}

	///////////////////////////////////////////////
	// Compute bounds
	u08 position_index = xen::findMeshAttrib(md, xen::VertexAttribute::Aspect::Position);

	if(position_index == xen::MeshData::BAD_ATTRIB_INDEX){
		if(md->bounds != xen::Aabb3r::MaxMinBox && md->bounds != xen::Aabb3r{Vec3r::Origin, Vec3r::Origin}){
			XenLogDebug("No known position attribute, using supplied bounds");
		} else {
			XenLogWarn("Mesh has no known position attribute and bounds were not supplied, using MaxMin box as bounds - this may effect culling efficiency");
			result->bounds = xen::Aabb3r::MaxMinBox;
		}
	} else {
		XenLogDebug("Computing mesh bounding box");
		const Vec3r* positions = (const Vec3r*)md->vertex_data[position_index];
		result->bounds.min = positions[0];
		result->bounds.max = positions[0];
		for(u32 i = 1; i < md->vertex_count; ++i){
			result->bounds.min = xen::min(result->bounds.min, positions[i]);
			result->bounds.max = xen::max(result->bounds.max, positions[i]);
		}
	}

	return result;
}

void xgl::updateMeshVertexData(const xen::Mesh* handle,
                               u32              attrib_index,
                               void*            new_data,
                               u32              start_vertex,
                               u32              end_vertex
                              ){
	xgl::MeshGlData* mesh = (xgl::MeshGlData*)handle;

	end_vertex = xen::min(end_vertex, mesh->vertex_count);
	if(end_vertex < start_vertex){ return; }

	xgl::VertexAttributeSource* attrib_source = &mesh->vertex_data[attrib_index];

	u32 attrib_size = xen::getVertexAttributeSize(mesh->vertex_spec[attrib_index]);

	if(attrib_source->buffer == 0){

		// Then no existing data for this attribute, so create a new one

		if(start_vertex != 0 || end_vertex != mesh->vertex_count){
			XenLogWarn("Updating mesh attrib %i but there is no existing data and the new data set is not complete",
			           attrib_index
			          );
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

void xgl::destroyMesh(const xen::Mesh* mesh){
	// :TODO: IMPLEMENT - currently resource link, GPU buffers needs destroying
	xen::freeType(xgl::state->pool_mesh, (xgl::MeshGlData*)mesh);
}

const xen::Mesh* xgl::createDynamicMesh(const xen::VertexSpec& vertex_spec,
                                        const u16 primitive_type,
                                        u32 max_vertex_count){
	// We assume that all attributes have per vertex data
	// (IE: not const for all verts)
	bool attrib_is_varying[255];
	for(unsigned i = 0; i < vertex_spec.size; ++i){ attrib_is_varying[i] = true; }

	xgl::MeshGlData* result = doCreateMesh(vertex_spec, primitive_type,
	                                       attrib_is_varying, max_vertex_count,
	                                       GL_DYNAMIC_DRAW);

	result->vertex_count    = 0;
	result->vertex_capacity = max_vertex_count;

	return result;
}

bool xgl::setDynamicMeshVertexCount(const xen::Mesh* handle, u32 vertex_count){
	auto mesh = (xgl::MeshGlData*)handle;

	if(mesh->vertex_capacity < vertex_count){ return false; }

	mesh->vertex_count = vertex_count;
	return true;
}

#endif
