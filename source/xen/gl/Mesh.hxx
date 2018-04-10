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

#include <cstring>

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
			VertexAttributeSource(){}


			VertexAttributeSource(const VertexAttributeSource& other){
				*this = other;
			}
			VertexAttributeSource& operator=(const VertexAttributeSource& other){
				memcpy(this, &other, sizeof(VertexAttributeSource));
				return *this;
			}

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

				/// \brief Value for 3 component real values when buffer is 0
				Vec3r   vec3r;

				/// \brief Value for 2 component float vector when buffer is 0
				Vec2f   vec2f;

				/// \brief Value for 3 component float color values when buffer is 0
				Color3f color3f;

				/// \brief Value for 4 component byte color values when buffer is 0
				Color   color4b;
			};
		};

		#pragma GCC diagnostic pop // re-enable -Wpedantic

		/////////////////////////////////////////////////////////////////////
		/// \brief Type representing a mesh stored in some set of GpuBuffers which
		/// can be rendered by OpenGL
		/////////////////////////////////////////////////////////////////////
		struct MeshGlData : public xen::MeshHeader{
			/// \brief The data sources for attributes of this mesh
			///
			/// Pointer to first element of array where each element is the source
			/// of the data for that attribute
			VertexAttributeSource* attrib_sources;
		};

		/// \brief Creates a mesh from segregated buffers of data for each attribute
		/// \param arena     Arena in which resulting MeshHeader instance is stored
		/// \param mesh_data The data for the Mesh as stored on the CPU
		MeshGlData* createMesh(ArenaLinear& arena, const MeshData& md);

		/// \brief Destroys a mesh, cleaning up all its resources
		//void destroyMesh(const char* mesh);
	}
}

#endif
