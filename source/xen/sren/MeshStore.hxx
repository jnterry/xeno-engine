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

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/vertex_group.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/memory/ArenaPool.hpp>

#include <cstring>

namespace xen {
namespace sren {


struct MeshBase : public MeshHeader, MeshAttribArrays{
	// Nothing else in common to both raytracer and rasterizer meshes...
};

/////////////////////////////////////////////////////////////////////
/// \brief Class which manages the meshes owned by some software device
///
/// \tparam T_MESH The type of the mesh object to manage. It is expected
/// that T_MESH is derived from MeshBase
/////////////////////////////////////////////////////////////////////
template<typename T_MESH>
class MeshStore {
private:
	/// \brief The device which owns this MeshStore
	xen::GraphicsDevice*   owner;

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
	MeshStore(xen::GraphicsDevice* owner, xen::Allocator* alloc)
		: owner(owner),
		  mesh_allocator(alloc),
		  mesh_pool(xen::createArenaPool<T_MESH>(alloc, 1024)) {
		// no-op
	}

	/// \brief Retrieves a mesh from the store by handle
	inline T_MESH* getMesh(xen::Mesh mesh) {
		return &this->mesh_pool.slots[mesh._id].item;
	}

	xen::Mesh createMesh          (const xen::MeshData* mesh_data);

	void      destroyMesh         (xen::Mesh mesh);

	void      updateMeshAttribData(xen::Mesh mesh,
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
  xen::fillMeshAttribArrays(mesh_geom, mesh_data, mesh_allocator);
  mesh_geom->vertex_count = mesh_data->vertex_count;

  ////////////////////////////////////////////////////////////////////////////
  // Compute face normals
  if(mesh_geom->normal == nullptr && mesh_data->vertex_count % 3 == 0){
	  for(u32 i = 0; i < mesh_geom->vertex_count; i += 3){
		  mesh_geom->normal = (Vec3r*)mesh_allocator->allocate
			  (sizeof(Vec3r) * mesh_geom->vertex_count);

		  xen::Triangle3r* tri = (xen::Triangle3r*)&mesh_geom->position[i];
		  Vec3r normal = xen::computeNormal(*tri);
		  mesh_geom->normal[i+0] = normal;
		  mesh_geom->normal[i+1] = normal;
		  mesh_geom->normal[i+2] = normal;
	  }
  }

  xen::Mesh handle;
  handle._id         = slot;
  handle._generation = 0;
  handle._device     = this->owner->getId();
  return handle;
}

template<typename T_MESH>
void MeshStore<T_MESH>::destroyMesh(xen::Mesh mesh) {
	xen::freeMeshAttribArrays(this->getMesh(mesh), mesh_allocator);
	xen::freeSlot(this->mesh_pool, mesh._id);
}

template<typename T_MESH>
void MeshStore<T_MESH>::updateMeshAttribData(xen::Mesh mesh_handle,
                                             u32 attrib_index,
                                             void* new_data,
                                             u32 start_vertex,
                                             u32 end_vertex
                                             ) {
	T_MESH* mesh = this->getMesh(mesh_handle);;

	end_vertex = xen::min(end_vertex, mesh->vertex_count);
	if(end_vertex < start_vertex){ return; }

	void** attrib_data = nullptr;
	switch(mesh->attrib_types[attrib_index]){
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

	u32 attrib_size = xen::getVertexAttributeSize(mesh->attrib_types[attrib_index]);
	if(*attrib_data == nullptr){
		*attrib_data = mesh_allocator->allocate(attrib_size * mesh->vertex_count);
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

} // end of namespace sren
} // end of namespace xen

#endif
