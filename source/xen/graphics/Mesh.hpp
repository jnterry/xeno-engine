////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of Mesh and related types and functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_HPP
#define XEN_GRAPHICS_MESH_HPP

#include <xen/config.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	struct ArenaLinear;

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

	/// \brief Full specification for the vertices of some mesh
	typedef xen::Array<VertexAttribute::Type> VertexSpec;

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the size of a VertexAttributeType in bytes
	/////////////////////////////////////////////////////////////////////
	u32 getVertexAttributeSize(VertexAttribute::Type type);

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents mesh data stored in main memory. This allows for
	/// manipulations to be performed by the CPU, but the data IS NOT
	/// in an appropriate format for rendering systems to draw the mesh
	/////////////////////////////////////////////////////////////////////
	struct MeshData {
		/// \brief The number of attributes this Mesh has
		u08                    attrib_count;

		/// \brief The types of each mesh attribute
		VertexAttribute::Type* attrib_types;

		/// \brief The data for each of this mesh's attributes
		/// Array of length attrib_count, where each
		void**                 attrib_data;

		/// \brief Number of vertices in the mesh
		u32                    vertex_count;
	};

	/// \brief Additional flags which modify how mesh loading is performed
	struct MeshLoadFlags {
		enum Values{
			NONE            = 0x00,

			/// \brief Indicates that smooth normals should be generated such that normal
			/// of each vertex is average of all faces the vertex is a part of
			//SMOOTH_NORMALS  = 0x01,

			/// \brief Modifies vertex positions such that center of mesh (according to bounding box)
			/// is at the origin
			//CENTER_ORIGIN   = 0x02,
		};
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a MeshData instance for the specified VertexSpec
	///
	/// All attribute data pointers will be set to nullptr
	///
	/// \note The returned MeshData instance will be self-contained within
	/// the arena, that is, the arrays for attrib_types and attrib_data
	/// will also be placed in the arena. Ownership of the memory for parameters
	/// to this function is not changed by this function.
	///
	/// \param arena The arena to which the MeshData instance as well as
	/// associated attrib_type array and attrib_data array will be pushed
	///
	/// \param spec The VertexSpec that the created MeshData should adhere to
	///
	/// \return Created MeshData instance
	/////////////////////////////////////////////////////////////////////
	MeshData* createEmptyMeshData(ArenaLinear& arena, const VertexSpec& spec);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an obj file in order to fill in the data for a MeshData
	/// instance.
	///
	/// This function will not overwrite any existing data for the Mesh, thus
	/// the attrib_data pointers in the MeshData for any attributes you wish
	/// to load from the obj file must be set to nullptr.
	///
	/// \note Obj files contain at most the aspects:
	/// - position
	/// - normal
	/// - uv coordinate
	/// The attrib_data pointers for vertex attributes for any other aspect
	/// will not be changed by this function
	///
	/// \warn This function assumes the file specified by `path` exists and
	/// will XenBreak otherwise
	///
	/// \param result MeshData instance to fill in to represent the loaded
	/// data. The attrib_count and attrib_types fields should already be set up
	/// as desired.
	///
	/// \param arena Arena in which to store the loaded vertex data
	///
	/// \param path The path of the file to load
	///
	/// \return True if mesh was successfully loaded, false otherwise. This can
	/// occur if the arena if not large enough to fully contain the meshes vertex
	/// data
	/////////////////////////////////////////////////////////////////////
  bool loadMeshObjFile(MeshData* result, ArenaLinear& arena, const char* path);
}

#endif
