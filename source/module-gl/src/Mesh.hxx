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
#include <xen/graphics/GraphicsHandles.hpp>

#include "gl_header.hxx"

#include <cstring>

namespace xgl {

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
			xen::Color3f color3f;

			/// \brief Value for 4 component byte color values when buffer is 0
			xen::Color   color4b;
		};
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Type representing a mesh stored in some set of GpuBuffers which
	/// can be rendered by OpenGL
	/////////////////////////////////////////////////////////////////////
	typedef xen::MeshDataSource<VertexAttributeSource> MeshGlData;
}

namespace xgl {
	xgl::MeshGlData* getMeshGlData(xen::Mesh mesh);
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