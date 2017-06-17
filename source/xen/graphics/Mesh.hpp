////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Mesh.hpp
/// \author Jamie Terry
/// \date 2017/06/17
/// \brief Contains types and functions for representing mesh's that can be rendered
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_HPP
#define XEN_GRAPHICS_MESH_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{

	/// \brief Opaque type representing buffer of data usable by the graphics hardware
	struct GpuBuffer;

	struct GpuBuffer{
		// :TODO: this should be implementation only
		GLuint handle;
	};

	/// \brief Represents a single attribute of a Vertex, eg position data, normal data, etc.
	/// Each vertex in a mesh will have a value for each attribute
	struct VertexAttrib{
		/// \brief Enumeration of the types of attributes of this mesh
		enum Type{
			PositionXYZ,  // \brief Vec3r representing (x,y,z) position of vertex

			NormalXYZ,    // \brief Vec3r representing (x,y,z) coords of the vertex

			ColorRGBf,    // \brief 3 floats between 0 and 1 representing color components of the vertex
		};

		/// \brief The type of this attribute
		Type type;

		/// \brief The delta (in bytes) between attributes of this type in the GPU buffer
		/// If set to 0 then assumed to be a constant value for all verticies, as given by
		/// the corresponding valueXXX field of the union below
		u32  stride;

		union{
			/// \brief Parameters describing layout of data in the GpuBuffer
			struct{
				u32 offset; /// \brief The offset of the first byte of one of these attributes in the GPU buffer
			};

			/// \brief The constant value of this attrib, if stride is 0 and this attrib is 3 component floating type
			Vec3r value3r;
		};
	};

	/// \brief Type representing a mesh stored in some GpuBuffer which can be rendered
	struct Mesh{

		Mesh(){}
		~Mesh(){}

		// :TODO: support indexed meshes

		// :TODO: support template param for type representing bounds min/max, so can
		// support 2d or 3d meshes (or would we just always use quads in 2d with transparency?

		GpuBuffer*   gpu_buffer;     /// \brief The GPU buffer containing this mesh's data
		u32          num_triangles;  /// \brief The number of triangles in this mesh
		Vec3r        bounds_min;     /// \brief The min point of the Aabb of this mesh
		Vec3r        bounds_max;     /// \brief The max point of the Aabb of this mesh
		//u08          attrib_count;   /// \brief The number of attributes per vertex of this mesh
		//VertexAttrib attributes[0];  /// \brief The types of attributes of this mesh
	};

	/// \brief The
	struct MeshLoadFlags {
		enum Values{
			NONE            = 0x00,

			/// \brief Indicates that smooth normals should be generated such that normal
			/// of each vertex is average of all faces the vertex is a part of
			SMOOTH_NORMALS  = 0x01,

			/// \brief Modifies vertex positions such that center of mesh (according to bounding box)
			/// is at the orgin
			CENTER_ORIGIN   = 0x02,
		};
	};

	/// \brief Loads a mesh from file
	//Mesh loadMesh(const char* const path, u32 flags = MeshLoadFlags::NONE);

	/// \brief Creates a mesh from segregated buffers of data for each attribute
	/// \param attrib_count The number of attributes for the final mesh
	/// \param attrib_types The types of each attribute of the mesh
	/// \param attrib_data  Array of void* pointing to first byte of buffers containing
	///                     the mesh data. If nullptr then will attempt to derive
	///                     the data, eg, generating normals from positions
	/// \param flags        Additional flags controling the way the mesh is created
	//Mesh createMesh(u08 attrib_count, u32 attrib_types, void** attrib_data, u32 flags = MeshLoadFlags::NONE);

	/// \brief Destroys a mesh, cleaning up all its resources
	//void destroyMesh(const char* mesh);
}

#endif
