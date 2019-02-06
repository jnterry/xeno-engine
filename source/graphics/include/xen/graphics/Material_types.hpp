////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains public API types used to interact with the material system
/// of a graphics API
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MATERIALTYPES_HPP
#define XEN_GRAPHICS_MATERIALTYPES_HPP

#include <xen/core/introspection.hpp>

namespace xen {

	struct MaterialParameterSource {
		enum Kind {
			/// \brief A dynamically settable parameter, will be included
			/// in the produced Material's "parameters" MetaType
		  Variable,

		  /// \brief The model matrix for the current object represented as a 4x4
		  /// matrix of reals
		  ModelMatrix,

		  /// \brief The view matrix for the current object represented as a 4x4
		  /// matrix of reals. Thus transforms from world space to camera space.
		  ViewMatrix,

		  /// \brief The matrix which projects from camera space to clip space,
		  /// represented as a 4x4 matrix of reals
		  ProjectionMatrix,

		  /// \brief Combined model-view-projection matrix that transforms from
		  /// model space to clip space
		  MvpMatrix,

		  /// \brief Combined view-projection matrix that transforms from world space
		  /// to clip space
		  VpMatrix,

		  /// \brief 3d position of the camera in world space
		  CameraWorldPosition,

		  /// \brief Normalised direction vector representing the direction in which
		  /// the camera is looking
		  CameraLookDirection,

		  /// \brief Normalised direction vector representing what direction is
		  /// considered "up" by the camera, IE: which direction objects must lie
		  /// in for them to be rendered above the center of the screen
		  CameraUpDir,

		  /// \brief 4d vector representing rgb and intensity of the ambient light
		  /// for the scene
		  AmbientLightColor,

		  /// \brief 3d float vector representing the position of a point light in
		  /// the scene. Note that the renderer is responsible for determine which
		  /// point lights are most prominent for some given object. If this is used
		  /// as the source of an array of material parameters the renderer will
		  /// pick the n most prominent such lights
		  PointLightPosition,

		  ///\ brief 4d vector representing rgb and intensity of a point light
		  PointLightColor,

		  /// \brief 3d vector representing constant, linear and squared falloff
		  /// of light intensity as a function of distance
		  PointLightAttenuation,

		  TextureChannel0,
		  TextureChannel1,
		  TextureChannel2,
		  TextureChannel3,

		  /// \brief Floating parameter representing number of seconds since kernel
		  /// creation
		  Runtime,
		};

		/// \brief The name of the parameter in question
		const char* name;

		/// \brief Which input source to use for the parameter
		Kind kind;
	};

	/// \brief Represents some material registered with the graphics API  which
	/// may be used to render geometry
	///
	/// For those familiar with OpenGL, a Material is simply a shader program with
	/// associated state representing how to fill in the values of uniform
	/// variables. Note however that Xeno Engine does not expose the concept
	/// of "Shader Programs" publicly since other rendering backends may be
	/// implemented differently
	struct Material {
		/// \brief MetaType which describes the expected set of parameters to be
		/// passed to the rendering backend when using this material
		/// Will be nullptr if there are zero parameters to be passed to the
		/// backend
		MetaType* parameters;

		// :TODO: mapping for VertexSpec? Currently we rely on user supplied
		// vertex spec when creating meshes and just hope this matches that
		// expected by the shader. If we said which var names mapped to which
		// aspects we could generate the vertex spec automatically for each
		// material as part of the creation process
		//VertexSpec vertex_spec;
	};
}

#endif
