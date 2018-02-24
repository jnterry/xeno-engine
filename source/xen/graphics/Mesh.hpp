////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of Mesh and related types
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_HPP
#define XEN_GRAPHICS_MESH_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{

	// gcc doesn't like the anonomous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/// \brief Represents meta-data about a single attribute of a Mesh Vertex,
	/// for example vertex positions, normal's, etc.
	/// Each vertex in a mesh will have a value for each attribute - this may
	/// vary per vertex, or be constant for all vertices
	struct VertexAttrib{
		/// \brief Enumeration of the types of attributes of this mesh
		enum Type{
			/// \brief Vec3r representing (x,y,z) position of vertex
			PositionXYZ,

			/// \brief Vec3r representing (x,y,z) coords of the vertex
			NormalXYZ,

			/// \brief 3 floats between 0 and 1 representing color components of the vertex
			ColorRGBf,
		};

		/// \brief The type of this attribute
		Type type;

		/// \brief The delta (in bytes) between attributes of this type in a buffer
		/// If set to 0 then this attribute is assumed to be a constant value for
		/// all vertices in a mesh, as given by
		/// the corresponding valueXXX field of the union below
		u32  stride;

		union{
			/// \brief Parameters describing layout of data in the GpuBuffer
			struct{
				uptr offset; /// \brief The offset of the first byte of one of these attributes in the GPU buffer
			};

			/// \brief The constant value of this attrib, if stride is 0 and this attrib is 3 component floating type
			Vec3r value3r;

			Vec3f value3f;
		};
	};
}

#endif
