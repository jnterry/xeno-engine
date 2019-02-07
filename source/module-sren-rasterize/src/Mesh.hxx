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

#include <xen/graphics/GraphicsHandles.hpp>

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

	/////////////////////////////////////////////////////////////////////
	/// \brief Fills a MeshAttribArrays struct with data, allocating space used
	/// by each array using the specified allocator and making a deep copy of the
	/// corresponding data in mesh_data
	///
	/// \note Any existing data in the MeshAttribArrays will be overwritten,
	/// this will cause a memory leak if any of the attribute are pointing to
	/// memory not managed by anything else!
	///
	/// \note The allocated memory may be freed by passing the MeshAttribArrays
	/// to freeMeshAttribArraysData
	///
	/// \param result    The MeshAttribArrays which will be initialised
	/// \param mesh_data The source of data to copy into the MeshAttribArrays
	/// \param allocator The allocator with which to get memory to store mesh data
	/// \param flags     Allows specifying additional flags when arrays are filled
	/// with data
	/////////////////////////////////////////////////////////////////////
	void fillMeshAttribArrays(MeshAttribArrays* result,
	                          const MeshData*   mesh_data,
	                          Allocator*        allocator,
	                          MeshLoadFlags     flags = 0
	                         );

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees any non-null pointers in the specified MeshAttribArrays
	/// by deallocating the memory using the specified Allocator
	/// \note This DOES NOT free the actual MeshAttribArrays instance
	/////////////////////////////////////////////////////////////////////
	void freeMeshAttribArrays(MeshAttribArrays* mesh,
	                          Allocator*        allocator);

	/////////////////////////////////////////////////////////////////////
	/// \brief Updates some subset of the data stored in a MeshAttribArrays
	/// instance. Treats it as if it were a flexible mesh type by working out
	/// which attribute to modify from a VertexSpec
	///
	/// \note The attribute specified by attrib_index must be one of the
	/// types of attribute stored in a MeshAttribArrays instance
	///
	/// \param mesh The mesh to modify
	/// \param alloc The allocator to use to allocate vertex data with
	/// if the attribute being set currently has a nullptr buffer
	/// \param vertex_spec List of attributes in the mesh
	/// \param attrib_index The index of the attribute to modify
	/// \param data Pointer to new data set, must contain at least enough
	/// data to set (vertex_end - vertex_start) vertices
	/// \param vertex_start The first vertex to modify, defaults to 0
	/// \param vertex_end The last vertex to modify, defaults to max int, but
	/// will be clamped to the number of vertices in the mesh
	/////////////////////////////////////////////////////////////////////
	void setMeshAttribArraysData(MeshAttribArrays* mesh,
	                             xen::Allocator& alloc,
	                             const VertexSpec& vertex_spec,
	                             u32 attrib_index,
	                             void* data,
	                             u32 vertex_start = 0,
	                             u32 vertex_end   = 0xFFFFFFFF
	                            );
}

#endif
