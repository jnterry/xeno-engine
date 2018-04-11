////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of types for representing Meshes - IE:
/// collections of vertices with some number of attributes
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_TYPES_HPP
#define XEN_GRAPHICS_MESH_TYPES_HPP

#include <xen/config.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	struct ArenaLinear;

	// :TODO:REF:
	// Need a big clean up of the mesh code - was originally designed for
	// OpenGL only and then split into common code and that specific to GL
	// Theres a lot of concepts flying around, and conversions between them.
	// - Do we need flexible mesh data representations, as well as fixed
	//   representations
	//   - When meta type system is implemented can we use that for flexible
	//     representation?
	//   - Probably have a lot of repeated code
	// - Make Vertex Attrib / Attribute naming consistent
	// - VertexAttribute should be a bitfield so we can do
	//   .aspect, .type and .channels but still used the named constant
	//   approach so we dont have to support arbitary combinations. Do this
	//   using static instances (like Vec3r::Origin)

	/////////////////////////////////////////////////////////////////////
	/// \brief Container for pointers to in memory arrays of attribute data
	/// for some mesh
	///
	/// \note MeshVertexData should ususally be used in place of this type.
	/// This is used when the number of vertices is implicit, or is stored
	/// elsewhere
	///
	/// This struct is designed to represent a mesh with a fixed format - IE:
	/// the number and type of vertex attributes cannot be changed. Use the
	/// other types in this file for more flexibility - but more complexity
	/////////////////////////////////////////////////////////////////////
	struct MeshAttribArrays {
		/// \brief The positions of each vertex
		Vec3r* position;

		/// \brief The normals of each vertex
		Vec3r* normal;

		/// \brief The colors of each vertex
		Color* color;

		inline MeshAttribArrays(Vec3r* p, Vec3r* n, Color* c)
			: position(p), normal(n), color(c){
			// no-op
		}
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Struct representing the minimum amount of data to represent
	/// a Mesh stored in CPU memory. IE, a set of vertex arrays
	///
	/// This struct is designed to represent a mesh with a fixed format - IE:
	/// the number and type of vertex attributes cannot be changed. Use the
	/// other types in this file for more flexibility - but more complexity
	///
	/// \todo :TODO: better name?
	/////////////////////////////////////////////////////////////////////
	struct MeshGeometrySource : public MeshAttribArrays {
		/// \brief Number of vertices in this mesh. Length of any non nullptr
		/// attrib arrays is equal to this count.
		u32 vertex_count;

		inline MeshGeometrySource(u32 vertex_count, Vec3r* p, Vec3r* n, Color* c)
			: MeshAttribArrays(p, n, c), vertex_count(vertex_count){
			// no-op
		}
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents meta-data about the type of a Mesh vertex attribute.
	///
	/// These have an associated "aspect" that they attempt to capture, eg,
	/// "position", "normals", "texture coordinates", etc as well as an
	/// associated data type (eg, 3 channel floating)
	///
	/// Each vertex in a mesh will have a value for each attribute - this may
	/// vary per vertex, or be constant for all vertices of a mesh.
	/////////////////////////////////////////////////////////////////////
  struct VertexAttribute {
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
		  _TypeByte           = 0x00A0,
		  _TypeMax,
		  _TypeMask           = 0x00F8,

		  _AspectPosition     = 0x0100,
		  _AspectNormal       = 0x0200,
		  _AspectColor        = 0x0300,
		  //_AspectTangent      = 0x0400,
		  //_AspectBiTangent      = 0x0800,


		  // :TODO: -> if we have multiple textures we may want multiple texture
		  // coordinates. We could say if top bit of aspect is set then the aspect
		  // is uv coordinates, and then use the other 7 bits as a mask for which
		  // texture channels to apply a set of uv coordinates to
		  // But... what if we want more than 7 texture channels for a mesh?
		  // bump this to 32 bit?
		  _AspectTexCoord     = 0x8000,
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

		  /// \brief 4 component RGBA color with each component being a byte
		  /// between 0 and 255
		  Color4b    = _AspectColor    | _TypeByte  | 4,

		  /// \brief 2 component floating point texture coordinate between 0 and 1
		  TexCoord2f = _AspectTexCoord | _TypeFloat | 2,
	  };
  };

	typedef xen::Array<xen::VertexAttribute::Type> VertexSpec;


	/////////////////////////////////////////////////////////////////////
	/// \brief Meta data about some Mesh
	/////////////////////////////////////////////////////////////////////
	struct MeshHeader {
		/// \brief Index reserved to represent an invalid attribute index
		static const constexpr u08 BAD_ATTRIB_INDEX = 255;

		/// \brief The number of attributes this Mesh has
		u08                    attrib_count;

		/// \brief The types of each mesh attribute
		VertexAttribute::Type* attrib_types;

		/// \brief The bounding box of this mesh
		xen::Aabb3r            bounds;

		// :TODO: support indexed meshes
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents mesh data stored in main memory. This allows for
	/// manipulations to be performed by the CPU, but the data IS NOT
	/// in an appropriate format for rendering systems to draw the mesh
	/////////////////////////////////////////////////////////////////////
	struct MeshData : public MeshHeader {
		/// \brief The data for each of this mesh's attributes
		///
		/// Array of length attrib_count, where each element is a void*
		/// to the first byte of the data representing that attribute
		/// or nullptr if no data is stored for that attribute
		void** attrib_data;

		/// \brief Number of vertices in the mesh
		u32 vertex_count;
	};
}

#endif
