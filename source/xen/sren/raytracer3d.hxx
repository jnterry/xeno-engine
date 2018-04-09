////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of raytracer types and functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACER3D_HXX
#define XEN_SREN_RAYTRACER3D_HXX

#include <xen/math/geometry_types.hpp>

namespace xen {
	struct RenderParameters3d;

	namespace sren {
		struct RenderTargetImpl;

		struct RaytracerMesh : public MeshGeometrySource {
			// nothing here yet...
			//
			// - Bounding box to cull off screen meshes?
			// - can we sort the geometry to make rendering more efficient somehow?
			// - anything we can pre-compute?
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Represents the results from casting a ray out into the
		/// scene to be rendered
		/////////////////////////////////////////////////////////////////////
		struct SceneRayCastResult {
			/// \brief The position of the intersection in world space
			Vec3r pos_world;

			/// \brief The position of the intersection in model space
			Vec3r pos_model;

			/// \brief The square of the distance between the ray's origin and
			/// the intersection position in world space
			real dist_sq;

			/// \brief The index of the model with which the intersection occurred
			u32 cmd_index;

			/// \brief Which triangle of the target object the ray intersected
			u32 tri_index;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Casts a ray into some scene computing the ray's intersection
		/// with geometry
		/// \param ray_world The ray to cast, represented in world space
		/// \param commands  The set of rendering commands representing the scene
		/// to cast the ray into
		/// \param result    Data about the resulting intersection
		/// \return True if an intersection was found, false otherwise
		///
		/// \note Contents of result struct is undefined after this operation
		/// if false is returned
		/////////////////////////////////////////////////////////////////////
		bool castRayIntoScene(const xen::Ray3r& ray_world,
		                      const xen::Array<xen::RenderCommand3d>& commands,
		                      SceneRayCastResult& result);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param commands Array of render commands to perform
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (xen::sren::RenderTargetImpl&       target,
		                     const xen::Aabb2u&                 viewport,
		                     const xen::RenderParameters3d&     params,
		                     const xen::Array<RenderCommand3d>& commands);
	}
}

#endif
