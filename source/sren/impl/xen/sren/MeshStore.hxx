////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of the MeshStore type which can manage the
/// meshes owned by a software device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_MESHSTORE_HXX
#define XEN_SREN_MESHSTORE_HXX

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/math/vertex_group.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/array.hpp>

#include <cstring>

namespace xen {
namespace sren {

/////////////////////////////////////////////////////////////////////
/// \brief Class which manages the meshes owned by some software device
///
/// \tparam T_MESH The type of the mesh object to manage. It is expected
/// that T_MESH is derived from both MeshHeader and MeshAttribArrays
/////////////////////////////////////////////////////////////////////
template<typename T_MESH>
class MeshStore {
private:
	/// \brief The allocator for mesh attribute data
	xen::Allocator*        mesh_allocator;

	/// \brief The pool of meshes
	xen::ArenaPool<T_MESH> mesh_pool;
public:
	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new MeshStore which will use the specified allocator
	/// to allocate memory for the mesh data.
	///
	/// \note The MeshStore does not own the allocator passed in and will not
	/// attempt to free it upon destruction, it merely uses the allocator
	/////////////////////////////////////////////////////////////////////
	MeshStore(xen::Allocator* alloc)
		: mesh_allocator(alloc),
		  mesh_pool(xen::createArenaPool<T_MESH>(alloc, 1024)) {
		// no-op
	}

	/// \brief Retrieves a mesh from the store by handle
	inline T_MESH* getMesh(xen::Mesh mesh) {
		return &this->mesh_pool.slots[mesh._id].item;
	}

	xen::Mesh createMesh          (const xen::MeshData* mesh_data);

	void      destroyMesh         (xen::Mesh mesh);

	void      updateMeshVertexData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex);

	void destroyAllMeshes(){
		// :TODO: implement - resource leak
	}

}; // end of class MeshStore

template<typename T_MESH>
xen::Mesh MeshStore<T_MESH>::createMesh(const xen::MeshData* mesh_data) {
	// :TODO:COMP:ISSUE_31: object pool with automatic handles / resizeable pool
	u32 slot = xen::reserveSlot(this->mesh_pool);
  T_MESH* mesh_geom = &this->mesh_pool.slots[slot].item;

  // Copy over the common elements
  *((xen::MeshHeader*)mesh_geom) = *((xen::MeshHeader*)mesh_data);

  // Allocate storage and copy over attributes, this is equivalent
  // to uploading to the gpu in a gl device
  xen::fillMeshAttribArrays(mesh_geom, mesh_data, mesh_allocator,
                            xen::MeshLoadFlags::GENERATE_FLAT_NORMALS
                           );

  return xen::makeGraphicsHandle<xen::Mesh::HANDLE_ID>(slot, 0);
}

template<typename T_MESH>
void MeshStore<T_MESH>::destroyMesh(xen::Mesh mesh) {
	xen::freeMeshAttribArrays(this->getMesh(mesh), mesh_allocator);
	xen::freeSlot(this->mesh_pool, mesh._id);
}

template<typename T_MESH>
void MeshStore<T_MESH>::updateMeshVertexData(xen::Mesh mesh_handle,
                                             u32 attrib_index,
                                             void* new_data,
                                             u32 start_vertex,
                                             u32 end_vertex
                                            ) {
	T_MESH* mesh = this->getMesh(mesh_handle);
	xen::setMeshAttribArraysData(mesh,
	                             *mesh_allocator,
	                             mesh->vertex_spec, attrib_index,
	                             new_data,
	                             start_vertex, end_vertex
	                            );
}

} // end of namespace sren
} // end of namespace xen

#endif
