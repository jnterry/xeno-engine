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
#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{

	/// \brief Opaque type representing a memory buffer usable by the graphics device
	typedef void* GraphicsBuffer;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents meta-data about the type of a Mesh vertex attribute
	/// These have an associated "aspect" that they attempt to capture, eg,
	/// "position", "normals", "texture coordinates", etc as well as an
	/// associated data type (eg, 3 channel floating)
	///
	/// Each vertex in a mesh will have a value for each attribute - this may
	/// vary per vertex, or be constant for all vertices
	/////////////////////////////////////////////////////////////////////
	struct VertexAttributeType {
		// :TODO: split into "aspect" and "(data) type"
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
	};

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Stores meta data about where to find the vertex data for some
	/// attribute of a mesh
	/////////////////////////////////////////////////////////////////////
	struct VertexAttributeSource {
		/// \brief The buffer that the vertex attribute data is stored in
		/// If null then it is assumed all vertices have the same value
		/// for this attribute
		GraphicsBuffer buffer;

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
}

#endif
