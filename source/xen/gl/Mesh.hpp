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
#include <xen/graphics/VertexAttribute.hpp>

#include "gl_header.hxx"

namespace xen{
	namespace gl{
		// Disable gcc's warning about anonymous structs in unions temporarily...
		#pragma GCC diagnostic push
		#pragma GCC diagnostic ignored "-Wpedantic"

		/////////////////////////////////////////////////////////////////////
		/// \brief Stores meta data about where to find the vertex data for some
		/// attribute of a mesh
		/////////////////////////////////////////////////////////////////////
		struct VertexAttributeSource {
			/// \brief The buffer that the vertex attribute data is stored in
			/// If 0 then it is assumed all vertices have the same value
			/// for this attribute
			GLuint buffer;

			union {
				/// \brief Meta data for a varying attribute, IE: where buffer is not null
				struct {
					/// \brief The offset in bytes of the first byte in the buffer storing
					/// the value of this attribute
					uptr offset;

					/// \brief The delta in bytes between values of this attribute in the
					/// corresponding buffer
					uptr stride;
				};

				/// \brief Value for 3 component real values when buffer is null
				Vec3r   vec3r;

				/// \brief Value for 3 component float color values when buffer is null
				Color3f color3f;
			};
		};

		#pragma GCC diagnostic pop // re-enable -Wpedantic

		/// \brief Type representing a mesh stored in some GpuBuffer which can be rendered
		struct Mesh{
			// :TODO: support indexed meshes

			// :TODO: support template param for type representing bounds min/max, so can
			// support 2d or 3d meshes (or would we just always use quads in 2d with transparency?

			/// \brief The number of triangles in this mesh
			u32                    num_triangles;

			/// \brief The bounding box of the geometry for this mesh
			Aabb3r                 bounds;

			/// \brief The number of attributes per vertex of this mesh
			u08                    attribute_count;

			/// \brief The types of attributes of this mesh
			VertexAttribute::Type* attribute_types;

			/// \brief The data sources for attributes of this mesh
			VertexAttributeSource* attribute_sources;
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
		Mesh* createMesh(ArenaLinear&                 arena,
		                 u08                          attrib_count,
		                 const VertexAttribute::Type* attrib_types,
		                 const void**                 attrib_data,
		                 u32                          vertex_count,
		                 u32 flags = MeshLoadFlags::NONE);

		/// \brief Destroys a mesh, cleaning up all its resources
		//void destroyMesh(const char* mesh);
	}
}

#endif
