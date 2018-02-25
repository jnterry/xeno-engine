////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of Mesh and related types
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_HPP
#define XEN_GRAPHICS_MESH_HPP

#include <xen/config.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{

	/// \brief Opaque type representing a memory buffer usable by the graphics device
	typedef u32 GraphicsBuffer;

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

	  // A type is really split into:
	  // - aspect (position, normal, uv coord, etc)
	  // - channel type (float, double, int, etc)
	  // - number of channels
	  //
	  // However from an implementation perspective it is easier to only support
	  // certain combinations, eg, a Position is either a Vec2r or Vec3r, never
	  // a 4 component vector of bytes (which could be used to represent a color)
	  //
	  // However also from implementation perspective it can be helpful to write
	  // generic code that deals with any Vec3r type, or any position attribute.
	  //
	  // Additionally we may want to support arbitary types for arbitary aspects
	  // at some later type.
	  //
	  // Hence we will represent a VertexAttributeType as a named constant
	  // Position3r from a xeno engine user's perspective, but as a bit field
	  // from the implementation's perspective.
	  //
	  // Using 16 bits the masks are
	  // - channel_count : 0000 0000 0000 0111 (3 bits,          max:   8)
	  // - type          : 0000 0000 1111 1000 (5 bits, combinations:  32)
	  // - aspect        : 1111 1111 0000 0000 (8 bits, combinations: 256)

	  enum _Flags {
		  _ComponentCountMask = 0x0007,

		  _TypeFloat          = 0x0080,
		  _TypeDouble         = 0x0090,
		  _TypeMax,
		  _TypeMask           = 0x00F8,

		  _AspectPosition     = 0x0100,
		  _AspectNormal       = 0x0200,
		  _AspectTangent      = 0x0300,
		  _AspectColor        = 0x0400,

		  // :TODO: -> if we have multiple textures we may want multiple texture
		  // coordinates. We could say if top bit of aspect is set then the aspect
		  // is uv coordinates, and then use the other 7 bits as a mask for which
		  // texture channels to apply a set of uv coordinates to
		  // But... what if we want more than 7 texture channels for a mesh?
		  // bump this to 32 bit?
		  _AspectTextureCoord = 0x8000,
		  _AspectMax,

		  _AspectMask = 0xFF00,

			#if XEN_USE_DOUBLE_PRECISION
		  _TypeReal = _TypeDouble,
		  #else
		  _TypeReal = _TypeFloat,
		  #endif
	  };
	  static_assert(4 <= _ComponentCountMask,
	                "ComponentCountMask too small"
	               );
	  static_assert(_TypeMax   - 1 <= _TypeMask,
	                "Too many types to fit in bits allocated"
	               );
	  static_assert(_AspectMax - 1 <= _AspectMask,
	                "Too many aspects to fit in bits allocated"
	               );
	  static_assert((_ComponentCountMask ^ _TypeMask ^ _AspectMask) == 0xFFFF,
	                "Overlapping or incomplete masks"
	               );

	  enum Type {
		  /// \brief 3 component real vector representing xyz position of vertex
		  Position3r = _AspectPosition | _TypeReal  | 3,

		  /// \brief 3 component real vector representing normal direction
		  Normal3r   = _AspectNormal   | _TypeReal  | 3,

		  /// \brief 3 component RGB color with each component between 0 and 1
		  Color3f    = _AspectColor    | _TypeFloat | 3,
	  };
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

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the size of a VertexAttributeType in bytes
	/////////////////////////////////////////////////////////////////////
	u32 getVertexAttributeTypeSize(VertexAttributeType::Type type);

	/////////////////////////////////////////////////////////////////////
	/// \brief Gets the default source for a vertex attribute of the specified
	/// type. This will be a constant with some sensible value dependent on
	/// the vertex attribute type's aspect
	/////////////////////////////////////////////////////////////////////
	VertexAttributeSource getDefaultVertexAttributeSource(xen::VertexAttributeType::Type type);

}

#endif
