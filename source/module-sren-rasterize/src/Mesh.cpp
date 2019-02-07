////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MESH_CPP
#define XEN_MODULESRENRASTERIZE_MESH_CPP

#include <xen/sren/rasterizer3d.hxx>
#include "Mesh.hxx"
#include "ModuleRasterize.hxx"

xsr::RasterizerMesh* xsr::getMeshImpl(xen::Mesh mesh){
	return &xsr::state->mesh_pool.slots[mesh._id].item;
}

xen::Mesh xsr::createMesh(const xen::MeshData* mesh_data) {
	u32 slot = xen::reserveSlot(xsr::state->mesh_pool);
	auto* mesh_geom = &xsr::state->mesh_pool.slots[slot].item;

  // Copy over the common elements
  *((xen::MeshHeader*)mesh_geom) = *((xen::MeshHeader*)mesh_data);

  // Allocate storage and copy over attributes, this is equivalent
  // to uploading to the gpu in a gl device
  xen::fillMeshAttribArrays(mesh_geom, mesh_data, xsr::state->mesh_attrib_alloc,
                            xen::MeshLoadFlags::GENERATE_FLAT_NORMALS
                           );

  return xen::makeGraphicsHandle<xen::Mesh::HANDLE_ID>(slot, 0);
}

void xsr::destroyMesh(xen::Mesh mesh) {
	xen::freeMeshAttribArrays(xsr::getMeshImpl(mesh), xsr::state->mesh_attrib_alloc);
	xen::freeSlot(xsr::state->mesh_pool, mesh._id);
}

void xsr::updateMeshVertexData(xen::MeshAttribArrays* mesh_geom,
                               const MeshData*   mesh_data,
                               xen::Allocator*        allocator,
                               xen::MeshLoadFlags     flags
){
	xen::clearToZero(mesh_geom);
	mesh_geom->vertex_count = mesh_data->vertex_count;

	VertexAttributeAspects aspect = findVertexSpecAspectIndices(mesh_data->vertex_spec);


	XenAssert(aspect.position != xen::MeshData::BAD_ATTRIB_INDEX,
	          "Mesh must have position attribute");
	XenAssert(mesh_data->vertex_data[aspect.position] != nullptr,
	          "Data source for position attribute must be non-null"
	);
	{
		XenAssert((mesh_data->vertex_spec[aspect.position] &
		           xen::VertexAttribute::_TypeMask
		          ) == xen::VertexAttribute::_TypeReal,
		          "Expected position components to be reals"
		);
		XenAssert((mesh_data->vertex_spec[aspect.position] &
		           xen::VertexAttribute::_ComponentCountMask
		          ) == 3,
		          "Expected position attribute to have 3 channels"
		);
		u32 byte_count_pos  = sizeof(Vec3r) * mesh_data->vertex_count;
		mesh_geom->position = (Vec3r*)allocator->allocate(byte_count_pos);
		memcpy(mesh_geom->position, mesh_data->vertex_data[aspect.position], byte_count_pos);
	}

	if(aspect.normal != xen::MeshData::BAD_ATTRIB_INDEX &&
	   mesh_data->vertex_data[aspect.normal] != nullptr
	){
		XenAssert((mesh_data->vertex_spec[aspect.normal] &
		           xen::VertexAttribute::_TypeMask
		          ) == xen::VertexAttribute::_TypeReal,
		          "Expected normal components to be reals"
		);
		XenAssert((mesh_data->vertex_spec[aspect.normal] &
		           xen::VertexAttribute::_ComponentCountMask
		          ) == 3,
		          "Expected normal attribute to have 3 channels"
		);
		u32 byte_count_nor  = sizeof(Vec3r) * mesh_data->vertex_count;
		mesh_geom->normal   = (Vec3r*)allocator->allocate(byte_count_nor);
		memcpy(mesh_geom->normal, mesh_data->vertex_data[aspect.normal], byte_count_nor);
	}

	if(aspect.texcoord != xen::MeshData::BAD_ATTRIB_INDEX &&
	   mesh_data->vertex_data[aspect.texcoord] != nullptr
	){

		XenAssert((mesh_data->vertex_spec[aspect.texcoord] &
		           xen::VertexAttribute::_TypeMask
		          ) == xen::VertexAttribute::_TypeFloat,
		          "Expected texcoord components to be reals"
		);
		XenAssert((mesh_data->vertex_spec[aspect.texcoord] &
		           xen::VertexAttribute::_ComponentCountMask
		          ) == 2,
		          "Expected texcoord attribute to have 2 channels"
		);
		u32 byte_count_uvs  = sizeof(Vec2f) * mesh_data->vertex_count;
		mesh_geom->uvs   = (Vec2f*)allocator->allocate(byte_count_uvs);
		memcpy(mesh_geom->uvs, mesh_data->vertex_data[aspect.texcoord], byte_count_uvs);
	}

	if(aspect.color != xen::MeshData::BAD_ATTRIB_INDEX &&
	   mesh_data->vertex_data[aspect.color] != nullptr
	){
		u32 byte_count_color  = sizeof(xen::Color) * mesh_data->vertex_count;
		mesh_geom->color      = (xen::Color*)allocator->allocate(byte_count_color);

		switch(mesh_data->vertex_spec[aspect.color]){
		case xen::VertexAttribute::Color3f: {
			xen::Color3f* src_buf = (xen::Color3f*)mesh_data->vertex_data[aspect.color];
			for(u32 i = 0; i < mesh_data->vertex_count; ++i){
				mesh_geom->color[i] = xen::makeColor(src_buf[i]);
			}
			break;
		}
		case xen::VertexAttribute::Color4b: {
			memcpy(mesh_geom->color, mesh_data->vertex_data[aspect.color], byte_count_color);
			break;
		}
		default:
			XenInvalidCodePath("Found bad color format in mesh");
		}
	}

	XenAssert(flags == 0 || flags == MeshLoadFlags::GENERATE_FLAT_NORMALS,
	          "Other flags not yet implemented"
	);

	if((flags & MeshLoadFlags::GENERATE_FLAT_NORMALS) != 0 &&
	   mesh_geom->normal == nullptr &&
	   mesh_geom->vertex_count % 3 == 0){
		mesh_geom->normal = (Vec3r*)allocator->allocate
			(sizeof(Vec3r) * mesh_geom->vertex_count);
		xen::computeFlatNormals(mesh_geom->vertex_count, mesh_geom->position, mesh_geom->normal);
	}

}

void xsr::freeMeshAttribArrays(xen::MeshAttribArrays* mesh,
                               xen::Allocator* allocator){
	if(mesh->position != nullptr){
		allocator->deallocate(mesh->position);
		mesh->position = nullptr;
	}

	if(mesh->normal != nullptr){
		allocator->deallocate(mesh->normal);
		mesh->normal = nullptr;
	}

	if(mesh->color != nullptr){
		allocator->deallocate(mesh->color);
		mesh->color = nullptr;
	}

	if(mesh->uvs != nullptr){
		allocator->deallocate(mesh->uvs);
		mesh->uvs = nullptr;
	}
}

void xen::setMeshAttribArraysData(xen::Mesh mesh_handle,
                                  xen::Allocator& alloc,
                                  const xen::VertexSpec& vertex_spec,
                                  u32 attrib_index,
                                  void* new_data,
                                  u32 start_vertex,
                                  u32 end_vertex
                                 ){

	auto* mesh = xsr::getMeshImpl(mesh_handle);

	end_vertex = xen::min(end_vertex, mesh->vertex_count);
	if(end_vertex < start_vertex){ return; }

	void** attrib_data = nullptr;
	switch(vertex_spec[attrib_index]){
	case xen::VertexAttribute::Position3r:
		attrib_data = (void**)&mesh->position;
		break;
	case xen::VertexAttribute::Normal3r:
		attrib_data = (void**)&mesh->normal;
		break;
	case xen::VertexAttribute::Color4b:
		attrib_data = (void**)&mesh->color;
		break;
	default:
		XenInvalidCodePath("Attempt to update unsupported mesh attribute type");
		return;
	}

	u32 attrib_size = xen::getVertexAttributeSize(vertex_spec[attrib_index]);
	if(*attrib_data == nullptr){
		*attrib_data = alloc.allocate(attrib_size * mesh->vertex_count);
		if(start_vertex != 0 || end_vertex != mesh->vertex_count){
			// :TODO: log
			printf("WARN: Updating mesh attrib %i but there is no existing data and "
			       "the new data set is not complete\n", attrib_index);
		}
	}

	memcpy(xen::ptrGetAdvanced(*attrib_data, start_vertex * attrib_size),
	       new_data,
	       (end_vertex - start_vertex) * attrib_size
	       );

}

#endif
