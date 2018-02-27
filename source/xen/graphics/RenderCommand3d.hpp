////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for defining 3d render commands
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_RENDERER3D_HPP
#define XEN_GRAPHICS_RENDERER3D_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/array_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Light3d.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/GraphicsDevice_types.hpp>

namespace xen{

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a single object to be rendered to the scene
	/////////////////////////////////////////////////////////////////////
	struct RenderCommand3d {
		enum Types {
			/// \brief Draws a set of points defined by `vertices` member
			POINTS,

			/// \brief Draws a set of points defined by `vertices` member
			/// by joining subsequent pairs of points
			LINES,

			/// \brief Draws a set of points defined by `vertices` member
			/// by joining adjacent points
			LINE_STRIP,

			/// \brief Draws a set of triangles defined by `vertices` member
			/// by grouping every 3 vertices
			TRIANGLES,

			/// \brief :TODO: temporary, render a mesh handle as triangles
			/// we really want to be able to render an actual mesh as
			/// different primitive types
			MESH,

			COUNT,
		};

		/// \brief Defines the type of render command represented
		u8 type;

		/// \brief The diffuse color to use
		Color4f color;

		/// \brief The emissive color of the surface, a/w component is interpreted
		/// as a brightness modifier
		Color4f emissive_color;

		/// \brief Matrix to transform from world space to model space
		Mat4r model_matrix;

		/// \brief Extra type dependent data
		union {
			struct {
				Vec3r* verticies;
				u32    count;
			} verticies;

		  Mesh mesh;
		};
	};

	/// \brief Extra parameters required to render a scene
	struct RenderParameters3d {
		RenderParameters3d();

		/// \brief The camera with which to render to scene
		Camera3d                  camera;

		/// \brief The ambient lighting to be added to all objects in the scene
		Color3f                   ambient_light;

		/// \brief Array of light sources in the scene
		xen::Array<LightSource3d> lights;

		// delta time for animations (or per animated object?)
		// skybox? -> useful as parameter to scene for reflections
		// lens flare?
		// fog?
		// quality selection? (ray tracer faster/more accurate)
	};

}

#endif
