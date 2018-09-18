////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions related to meshes for the software
/// rasterizer
///
/// \ingroup module-sren-rasterize
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MESH_HXX
#define XEN_MODULESRENRASTERIZE_MESH_HXX

#include <xen/graphics/GraphicsDevice_types.hpp>

namespace xsr {
	struct RasterizerMesh;

	xsr::RasterizerMesh* getMeshImpl(xen::Mesh mesh);

	xen::Mesh createMesh (const xen::MeshData* mesh_data);
	void      destroyMesh(xen::Mesh mesh);
	void      updateMeshVertexData(xen::Mesh mesh,
	                               u32 attrib_index,
	                               void* new_data,
	                               u32 start_vertex,
	                               u32 end_vertex
	                              );
}

#endif
