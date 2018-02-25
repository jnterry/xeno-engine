////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for representing mesh's that can be
/// rendered
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_MESH_HPP
#define XEN_GL_MESH_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/graphics/Mesh.hpp>

#include "gl_header.hxx"

namespace xen{
	namespace gl{
		/// \brief Type representing a mesh stored in some GpuBuffer which can be rendered
		struct Mesh{
			// :TODO: support indexed meshes

			// :TODO: support template param for type representing bounds min/max, so can
			// support 2d or 3d meshes (or would we just always use quads in 2d with transparency?

			/// \brief The number of triangles in this mesh
			u32                        num_triangles;

			/// \brief The bounding box of the geometry for this mesh
			Aabb3r                     bounds;

			/// \brief The number of attributes per vertex of this mesh
			u08                        attribute_count;

			/// \brief The types of attributes of this mesh
			VertexAttributeType::Type* attribute_types;

			/// \brief The data sources for attributes of this mesh
			VertexAttributeSource*     attribute_sources;
		};

		#pragma GCC diagnostic pop // re-enable -Wpedantic


		/// \brief The
		struct MeshLoadFlags {
			enum Values{
				NONE            = 0x00,

				/// \brief Indicates that smooth normals should be generated such that normal
				/// of each vertex is average of all faces the vertex is a part of
				//SMOOTH_NORMALS  = 0x01,

				/// \brief Modifies vertex positions such that center of mesh (according to bounding box)
				/// is at the orgin
				//CENTER_ORIGIN   = 0x02,
			};
		};

		/// \brief Loads a mesh from file
		/// \param arena Arena in which resulting Mesh instance is stored
		Mesh* loadMesh(xen::ArenaLinear& arena, const char* const path, u32 flags = MeshLoadFlags::NONE);

		/// \brief Creates a mesh from segregated buffers of data for each attribute
		/// \param arena        Arena in which resulting Mesh instance is stored
		/// \param attrib_count The number of attributes for the final mesh
		/// \param attrib_types The types of each attribute of the mesh
		/// \param vertex_count The number of verticies in the mesh, length of arrays pointed to by elements of attrib_data
		/// \param attrib_data  Array of void* pointing to first byte of buffers containing
		///                     the mesh data. If nullptr then will attempt to derive
		///                     the data, eg, generating normals from positions
		/// \param flags        Additional flags controling the way the mesh is created
		Mesh* createMesh(ArenaLinear&                     arena,
		                 u08                              attrib_count,
		                 const VertexAttributeType::Type* attrib_types,
		                 const void**                     attrib_data,
		                 u32                              vertex_count,
		                 u32 flags = MeshLoadFlags::NONE);

		/// \brief Destroys a mesh, cleaning up all its resources
		//void destroyMesh(const char* mesh);
	}
}

#endif
