////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of Mesh and related types and functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_HPP
#define XEN_GRAPHICS_MESH_HPP

#include <xen/graphics/Color.hpp>
#include <xen/graphics/Mesh_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>
#include <xen/config.hpp>

namespace xen{
	struct ArenaLinear;
	struct Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the size of a VertexAttributeType in bytes
	/////////////////////////////////////////////////////////////////////
	u32 getVertexAttributeSize(VertexAttribute::Type type);

	/// \brief Additional flags which modify how mesh loading is performed
	struct MeshLoadFlags {
		enum Values{
			NONE             = 0x00,

			/// \brief Indicates that normals should be generated if not present
			/// in the mesh file
			GENERATE_NORMALS = 0x01,

			/// \brief Indicates that smooth normals should be generated such that
			/// normal of each vertex is average of all faces the vertex is a part of
		  SMOOTH_NORMALS = 0x02,

			/// \brief Modifies vertex positions such that center of mesh (according to bounding box)
			/// is at the origin
			//CENTER_ORIGIN   = 0x04,
		};
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a MeshData instance for the specified VertexSpec
	///
	/// All attribute data pointers will be set to nullptr
	///
	/// \note The returned MeshData instance will be self-contained within
	/// the arena, that is, the arrays for attrib_types and attrib_data
	/// will also be placed in the arena. Ownership of the memory for parameters
	/// to this function is not changed by this function.
	///
	/// \param arena The arena to which the MeshData instance as well as
	/// associated attrib_type array and attrib_data array will be pushed
	///
	/// \param spec The VertexSpec that the created MeshData should adhere to
	///
	/// \return Created MeshData instance
	/////////////////////////////////////////////////////////////////////
	MeshData* createEmptyMeshData(ArenaLinear& arena, const VertexSpec& spec);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads a mesh file in order to fill in the data for a MeshData
	/// instance. Uses assimp in the background, so can be any format that
	/// assimp can load.
	///
	/// This function will not overwrite any existing data for the Mesh, thus
	/// the attrib_data pointers in the MeshData for any attributes you wish
	/// to load from the obj file must be set to nullptr.
	///
	/// \param result MeshData instance to fill in to represent the loaded
	/// data. The attrib_count and attrib_types fields should already be set up
	/// as desired.
	///
	/// \param arena Arena in which to store the loaded vertex data
	///
	/// \param path The path of the file to load
	///
	/// \return True if mesh was successfully loaded, false otherwise. This can
	/// occur if the arena if not large enough to fully contain the meshes vertex
	/// data
	/////////////////////////////////////////////////////////////////////
	bool loadMeshFile(MeshData* result, ArenaLinear& arena, const char* path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Returns the index of the first attribute representing the
	/// specified aspect in some mesh
	/// \param mesh_data The MeshData to search within
	/// \param aspect Flag indicating the aspect being searched for
	/// \return Index of the attribute storing that aspect.
	/// Returns MeshData::BAD_ATTRIB_INDEX if no such attribute exists
	/////////////////////////////////////////////////////////////////////
	u08 findMeshAttrib(const MeshData* mesh_data, VertexAttribute::_Flags aspect);

	/////////////////////////////////////////////////////////////////////
	/// \brief Initialises a MeshGeometrySource from some MeshData instance
	///
	/// This creates a deep copy of all data, allocating space using the specified
	/// allocator
	///
	/// \note Any existing data in the MeshGeometrySource will be overwritten,
	/// make sure it isn't pointing to memory not managed by anything else
	/// or this will be a memory leak
	///
	/// \note The allocated memory may be freed by passing the MeshGeometrySource
	/// to freeMeshGeometrySourceData
	///
	/// \param result    The MeshGeometrySource which will be initialised
	/// \param mesh_data The source of data to copy into the MeshGeometrySource
	/// \param allocator The allocator with which to get memory to store mesh data
	/////////////////////////////////////////////////////////////////////
	void initMeshGeometrySource(MeshGeometrySource* result,
	                            const MeshData*     mesh_data,
	                            Allocator*          allocator
	                           );

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees any non-null pointers in the specified MeshGeometrySource
	/// by deallocating the memory using the specified Allocator
	/// \note This DOES NOT free the actual MeshGeometrySource instance
	/////////////////////////////////////////////////////////////////////
	void freeMeshGeometrySourceData(MeshGeometrySource* mesh,
	                                Allocator* allocator);
}

#endif
