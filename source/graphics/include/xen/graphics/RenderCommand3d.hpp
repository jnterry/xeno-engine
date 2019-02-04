////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for defining 3d render commands
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_RENDERCOMMANDS3D_HPP
#define XEN_GRAPHICS_RENDERCOMMANDS3D_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>
#include <xen/core/bits.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Material_types.hpp>
#include <xen/graphics/Light3d.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/GraphicsHandles.hpp>

namespace xen{

	/////////////////////////////////////////////////////////////////////
	/// \brief Enumeration of the types of primitive a GraphicsDevice
	/// is able to draw
	/////////////////////////////////////////////////////////////////////
	enum class PrimitiveType {
		/// \brief Draws geometry as a point cloud
		POINTS,

		/// \brief Draws geometry by connecting subsequent pairs of points
	  LINES,

		/// \brief Draws geometry by connecting all adjacent points
	  LINE_STRIP,

		// LINE_LOOP,

		/// \brief Draws geometry by forming triangles by grouping every 3
		/// vertices
	  TRIANGLES,

		// TRIANGLE_FAN
		// TRIANGLE_STRIP
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Extra parameters required to render a scene. These vary per
	/// frame rather than per object
	/////////////////////////////////////////////////////////////////////
	struct RenderParameters3d {
		/// \brief The camera with which to render to scene
		Camera3d                  camera;

		/// \brief The ambient lighting to be added to all objects in the scene
		Color3f                   ambient_light;

		/// \brief Array of light sources in the scene
		xen::Array<LightSource3d> lights;

		// delta time for animations (or per animated object?) or should a
		// GraphicsDevice just keep track of its own time?
		//  -> probably want to pass it in so game time can be manipulated, eg,
		//     slow motion, pause screen, etc
		//
		// quality selection? (ray tracer faster/more accurate) - or should this
		// be tracked by the GraphicsDevice?
		//
		// skybox? -> useful as parameter to scene for reflections
		// lens flare?
		// fog?
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a single rendering operation which will draw
	/// some object to the screen
	/////////////////////////////////////////////////////////////////////
	struct RenderCommand3d {
		/// \brief Matrix to transform from world space to model space
		Mat4r model_matrix;

		/// \brief A handle to a mesh to be drawn
		xen::Mesh mesh;

		/// \brief The type of primitive to draw the mesh as
		PrimitiveType primitive_type;

		/// \brief Array of texture channels to be used by this rendering operation
		xen::Texture textures[4];

		/// \brief The material to be used to render the geometry
		const xen::Material* material;

		/// \brief Block of data to be used as source of parameters for the material
		/// Layout of the data should be that described by ~material->parameters~.
		/// May be set to nullptr if the material has no parameters, or if there is
		/// a default value for all parameters that do exist, and you are happy to
		/// use the defaults
		void* material_params;

		/// \brief Enumeration of extra misc flags that may be set for a command
		enum Flags : u08 {

			/// \brief If set then the geometry rendered by this command
			/// will not block light, and hence will not cast shadows
			///
			/// \note Flag is to disable rather than enable since geometry should
			/// cast shadows by default and we want the struct to have sensible
			/// defaults if zero initialised
			DisableShadowCast = 1,

			// :TODO:
			// CullBackFace,
			// CullFrontFace,
		};

		/// \brief Extra flags for the command
		Flags flags;
	};

	template<typename T>
	inline void setMaterialParam(RenderCommand3d& cmd, const char* param_name, const T& value){
		xen::setField(cmd.material->parameters,
		              param_name,
		              cmd.material_params,
		              value);
	}
}

#endif
