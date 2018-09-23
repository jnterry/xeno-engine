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
#include <xen/sren/rasterizer3d.hxx>

namespace xen {
	struct RenderParameters3d;
}

namespace xsr {
	struct RenderTarget;

	// If we ever want to pre-compute things for raytracer we can change this
	// to use a different type. Currently raytracer relies on all the rasterizer
	// module stuff except for the actual render function, would need to change
	// that if this ever changed...
	typedef RasterizerMesh RaytracerMesh;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a single model in a RaytracerScene, IE: an instance
	/// of some RaytracerMesh with specified transform, etc
	/////////////////////////////////////////////////////////////////////
	struct RaytracerModel {
		const RaytracerMesh* mesh;

		/// \brief The diffuse color to use
		xen::Color4f color;

		/// \brief The emissive color of the surface, a/w component is interpreted
		/// as a brightness modifier
		xen::Color4f emissive_color;

		/// \brief Matrix to transform from world space to model space
		Mat4r model_matrix;

		/// \brief Inverse of the model_matrix
		Mat4r inv_model_matrix;

		/// \brief The aabb of the model in world space
		xen::Aabb3r aabb_world;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Complete set of data representing some scene which may be
	/// rendered by the raytracer
	/////////////////////////////////////////////////////////////////////
	struct RaytracerScene {
		/// \brief The models in the scene
		xen::Array<RaytracerModel> models;

		/// \brief Index of the first shadow caster in the models array.
		/// All models before this point DO NOT cast shadows. All those
		/// after do.
		u32 first_shadow_caster;
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
	/// \param skip_non_shadow_casters If true then any models that do not
	/// cast shadows will be skipped
	///
	/// \return True if an intersection was found, false otherwise
	///
	/// \note Contents of result struct is undefined after this function
	/// if it returns false
	/////////////////////////////////////////////////////////////////////
	bool castRayIntoScene(const xen::Ray3r&          ray_world,
	                      const xsr::RaytracerScene& scene,
	                      xsr::SceneRayCastResult&   result,
	                      bool                       skip_non_shadow_casters = false);

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs a set of render commands using software raytracer
	///
	/// \param target The RenderTarget to draw to
	///
	/// \param viewport The region of the RenderTarget that may be modified
	/// represented in pixel values
	///
	/// \param camera The 3d camera used to view the scene
	///
	/// \param scene  The scene to render
	///
	/// \param rendering_bounds The bounds within the target that should be
	/// rendered to. This allows this function to be called from multiple
	/// threads with the same viewport but different rendering_bounds in order
	/// to allow for multi-threaded raytracing. If not specified we assumes a
	/// single threaded renderer and hence use the full viewport
	/////////////////////////////////////////////////////////////////////
	void renderRaytrace (xsr::RenderTarget&   target,
	                     const xen::Aabb2u&             viewport,
	                     const xen::RenderParameters3d& params,
	                     const RaytracerScene&          scene,
	                     const xen::Aabb2u&             rendering_bounds);

	inline void renderRaytrace (xsr::RenderTarget&   target,
	                            const xen::Aabb2u&             viewport,
	                            const xen::RenderParameters3d& params,
	                            const RaytracerScene&          scene){
		renderRaytrace(target, viewport, params, scene, viewport);
	}
}
#endif
