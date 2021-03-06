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

			/// \brief Value to be used for attribute if buffer is 0
			Vec3d        vec3d;
			Vec3f        vec3f;
			Vec2d        vec2d;
			Vec2f        vec2f;
			xen::Color3f color3f;
			xen::Color   color4b;
		};
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Type representing a mesh stored in some set of GpuBuffers which
	/// can be rendered by OpenGL
	/////////////////////////////////////////////////////////////////////
	struct MeshGlData : public xen::Mesh {
		/// \brief Array of meta data about which GL buffer contains the mesh data
		xgl::VertexAttributeSource* vertex_data;

		/// \brief GPU buffer capacity for vertex data - will be set to 0 unless
		/// mesh was created using createDynamicMesh method
		u32 vertex_capacity;
	};
}

namespace xgl {
	const xen::Mesh* createMesh (const xen::MeshData* mesh_data);
	void destroyMesh(const xen::Mesh* mesh);
	void updateMeshVertexData(const xen::Mesh* handle,
	                          u32 attrib_index, void* new_data,
	                          u32 start_vertex, u32 end_vertex);
	const xen::Mesh* createDynamicMesh(const xen::VertexSpec& vertex_spec,
	                                   const u16 primitive_type,
	                                   u32 max_vertex_count);
	bool setDynamicMeshVertexCount(const xen::Mesh* handle, u32 vertex_count);
}

#endif
