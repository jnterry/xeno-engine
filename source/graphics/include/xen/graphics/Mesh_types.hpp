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
	/////////////////////////////////////////////////////////////////////
	/// \brief A Mesh is simply some collection of vertices where we store some
	/// set of data per vertex, each called a "VertexAttribute". This struct
	/// represents meta data about a single attribute.
	///
	/// Each attribute represents a particular "aspect" of the mesh, for example,
	/// position data, normal data, texture coordinates, etc. Additionally each
	/// aspect has a known type (for example, 3 channel floating)
	///
	/// Letting the engine know which attribute represents which aspect is useful
	/// as it allows automatic processing to be performed (eg, computing mesh
	/// bounding box for culling, computing normals from position data, etc),
	/// however it is also possible to represent arbitrary data using the "Custom"
	/// aspect - this will be passed unaltered to the Material doing the rendering
	///
	/// \note Graphics backends are not required to support all possible
	/// combinations of aspect and type, for example, storing position data as a
	/// 1d vector of bytes makes very little sense. As such in order to guarantee
	/// support users of Xenogin should use the static instances within this
	/// struct. The exception to this rule is the "Custom" aspect, for which all
	/// possible type combinations are supported (within reason, for example,
	/// technically due to bit format of this struct we can represent an attribute
	/// with up to 7 components in a vector, however most backends (eg, opengl)
	/// only support up to 4).
	/////////////////////////////////////////////////////////////////////
  struct VertexAttribute {
	  /// \brief Enumeration of aspects represented by a particular attribute
	  /// \note If the top bit is set then the attribute is a custom. The user
	  /// may set the bottom 7 bits to any value to distringuish between different
	  /// custom attributes
	  enum Aspect : u08{
		  Position,
		  Normal,
		  Color,
		  TexCoord,
		  // Tangent, // :TODO: methods to compute these
		  // BiTangent,

		  Custom     = 0b10000000,
	  };
	  Aspect aspect;

	  /// \brief Represents the type of attribute. Most significant 5 bits
	  /// represent the actual type (eg, float, int, byte, etc), with the
	  /// least significant 3 bits representing the number of channels
	  enum Type : u08 {
		  Float         = (u08)1 << 3,
		  Double        = (u08)2 << 3,
		  Byte          = (u08)3 << 3,

		  #if XEN_USE_DOUBLE_PRECISION
		  Real = Double,
		  #else
		  Real = Float,
		  #endif

		  ComponentTypeMask  = 0b11111000,
		  ComponentCountMask = 0b00000111,
	  };
		u08 type;

	  static const VertexAttribute Position3r;
	  static const VertexAttribute Normal3r;
	  static const VertexAttribute Color3f;
	  static const VertexAttribute Color4b;
	  static const VertexAttribute TexCoord2f;
  };

	/// \brief Represents full specification of the data stored for a single
	/// Vertex in a mesh
	typedef xen::Array<xen::VertexAttribute> VertexSpec;


	/////////////////////////////////////////////////////////////////////
	/// \brief Enumeration of the types of primitive a GraphicsDevice
	/// is able to draw
	/// \brief Bottom 10 bits represents arbitrary integer to be used
	/// to store number of vertices when "Patch" type is used. These bits
	/// are ignored when any other primitive type is used.
	/////////////////////////////////////////////////////////////////////
	enum PrimitiveType : u16 {
		Points    = 1 << 10, // Each vertex an independent point
		Lines     = 2 << 10, // Every group of 2 vertices are connected
		LineStrip = 3 << 10, // All adjacent vertices are connected

		// LineLoop,

		Triangles = 4 << 10, // Every group of 3 vertices are connected

		// TriangleFan,
		// TriangleStrip,

		Patch = 5 << 10, // Arbitrary number of vertices to be fed to Tessellation control

		TypeMask       = 0b1111110000000000,
		PatchCountMask = 0b0000001111111111,
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Meta data about some Mesh
	/////////////////////////////////////////////////////////////////////
	struct MeshHeader {
		/// \brief Index reserved to represent an invalid VertexAttribute index
		static const constexpr u08 BAD_ATTRIB_INDEX = 255;

		/// \brief Number of vertices in the mesh
		u32 vertex_count;

		/// \brief Type of primitive making up the mesh, see xen::PrimitiveType
	  u16 primitive_type;

		/// \brief The number of and type of attributes in this mesh
		VertexSpec  vertex_spec;

		/// \brief The bounding box of this mesh
		xen::Aabb3r bounds;

		// :TODO: support indexed meshes
	};

	/// \brief Handle to Mesh object that may be used by graphics device to
	/// actually draw geometry
	struct Mesh : public MeshHeader{
		// Nothing here (yet)
		//
		// But we don't want MeshData to be castable to Mesh which it would if
		// we just used MeshHeader as the handle to a drawable Mesh, hence this is
		// its own type (and we might want to add fields at some point)
	};

	struct MeshData : public MeshHeader {
		/// \brief The data for each of this mesh's attributes
		///
		/// Array of length vertex_spec.length, where each element is a
		/// pointer to a buffer containing the actual vertex data
	  void** vertex_data;
	};
}

inline bool operator==(xen::VertexAttribute a, xen::VertexAttribute b){
	return a.aspect == b.aspect && a.type == b.type;
}
inline bool operator!=(xen::VertexAttribute a, xen::VertexAttribute b){
	return a.aspect != b.aspect || a.type != b.type;
}

#endif
