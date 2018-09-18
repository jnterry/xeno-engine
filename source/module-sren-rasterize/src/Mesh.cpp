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

void xsr::updateMeshVertexData(xen::Mesh mesh_handle,
                                             u32 attrib_index,
                                             void* new_data,
                                             u32 start_vertex,
                                             u32 end_vertex
                                            ) {
	auto* mesh = xsr::getMeshImpl(mesh_handle);
	xen::setMeshAttribArraysData(mesh,
	                             *xsr::state->mesh_attrib_alloc,
	                             mesh->vertex_spec, attrib_index,
	                             new_data,
	                             start_vertex, end_vertex
	                            );
}

#endif
