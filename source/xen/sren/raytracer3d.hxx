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

		struct RaytracerMesh : public MeshHeader, MeshAttribArrays {
			// Anything else?
			// - can we sort the geometry to make rendering more efficient somehow?
			// - anything we can pre-compute?
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Represents a single model in a RaytracerScene, IE: an instance
		/// of some RaytracerMesh with specified transform, etc
		/////////////////////////////////////////////////////////////////////
		struct RaytracerModel {
			const RaytracerMesh* mesh;

			/// \brief The diffuse color to use
			Color4f color;

			/// \brief The emissive color of the surface, a/w component is interpreted
			/// as a brightness modifier
			Color4f emissive_color;

			/// \brief Matrix to transform from world space to model space
			Mat4r model_matrix;

			/// \brief Inverse of the model_matrix
			Mat4r inv_model_matrix;

			/// \brief The aabb of the model in world space
			Aabb3r aabb_world;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Complete set of data representing some scene which may be
		/// rendered by the raytracer
		/////////////////////////////////////////////////////////////////////
		struct RaytracerScene {
			/// \brief The models in the scene
			xen::Array<RaytracerModel> models;
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
			real dist_sq_world;

			/// \brief The index of the model with which the intersection occurred
			u32 model_index;

			/// \brief Which triangle of the target object the ray intersected
			u32 tri_index;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Casts a ray into some scene computing the ray's intersection
		/// with geometry
		///
		/// \param ray_world The ray to cast, represented in world space
		/// \param scene     The scene to cast the ray into
		/// \param result    Data about the resulting intersection
		///
		/// \return True if an intersection was found, false otherwise
		///
		/// \note Contents of result struct is undefined after this function
		/// if it returns false
		/////////////////////////////////////////////////////////////////////
		bool castRayIntoScene(const xen::Ray3r& ray_world,
		                      const RaytracerScene& scene,
		                      SceneRayCastResult& result);

		/////////////////////////////////////////////////////////////////////
		/// \brief Performs a set of render commands use software raytracer
		/// \param target The RenderTarget to draw to
		/// \param viewport The region of the RenderTarget that may be modified
		/// Represented in pixel values
		/// \param camera The 3d camera used to view the scene
		/// \param scene  The scene to render
		/////////////////////////////////////////////////////////////////////
		void renderRaytrace (xen::sren::RenderTargetImpl&       target,
		                     const xen::Aabb2u&                 viewport,
		                     const xen::RenderParameters3d&     params,
		                     const RaytracerScene&              scene);
	}
}

#endif
