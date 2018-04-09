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
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/graphics/Light3d.hpp>
#include <xen/graphics/Camera3d.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/graphics/GraphicsDevice_types.hpp>

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
	/// \brief Represents a single rendering operation which will draw
	/// some object to the screen
	/////////////////////////////////////////////////////////////////////
	struct RenderCommand3d {
		/// \brief Enumeration of the possible sources of geometry for a rendering
		/// operation
		enum GeometrySource {
			// Do we really want to support immediate mode rendering?
			// its slow, but useful for testing
			IMMEDIATE,

			MESH,

			// :TODO: Implicit surfaces
		};

		/// \brief The diffuse color to use
		Color4f color;

		/// \brief The emissive color of the surface, a/w component is interpreted
		/// as a brightness modifier
		Color4f emissive_color;

		/// \brief Matrix to transform from world space to model space
		Mat4r model_matrix;

		/// \brief The type of primitive to be drawn by this command
		PrimitiveType primitive_type;

		/// \brief The source of the geometry for this command
		GeometrySource geometry_source;

		/// \brief Extra data dependent on the source field
		union {
			/// \brief Extra data used if source is Immediate
			xen::MeshGeometrySource immediate;

			/// \brief A handle to a mesh to be drawn, used if source is Mesh
			xen::Mesh mesh;
		};
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Extra parameters required to render a scene. These vary per
	/// frame rather than per object
	/////////////////////////////////////////////////////////////////////
	struct RenderParameters3d {
		RenderParameters3d();

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

}

#endif
