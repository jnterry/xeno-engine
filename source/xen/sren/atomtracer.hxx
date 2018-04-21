////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of atom tracer types and functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_ATOMTRACER_HXX
#define XEN_SREN_ATOMTRACER_HXX

#include "MeshStore.hxx"
#include "RenderTargetImpl.hxx"
#include "rasterizer3d.hxx"

#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/core/intrinsics.hpp>

namespace xen {
namespace sren {

/// \brief Output data from the scene atomiser
struct AtomizerOutput {
	struct Box {
		/// \brief The bounding box of this box
		xen::Aabb3r bounds;

		/// \brief The index of the first atom in this box
		u64    start;

		/// \brief The index one past the last atom in this box
		u64    end;
	};

	/// \brief The locations of atoms in the scene
	xen::Array<Vec3r> atoms;

	/// \brief Boxes that divide up the world's atoms spatially
	xen::Array<Box>   boxes;
};

AtomizerOutput& atomizeScene(const xen::Aabb2u& viewport,
                             const xen::RenderParameters3d& params,
                             const xen::Array<xen::RenderCommand3d>& commands,
                             xen::sren::MeshStore<xen::sren::RasterizerMesh>& mesh_store,
                             xen::ArenaLinear& arena,
                             real  atoms_per_pixel = 1.0_r);

struct RayPointIntersection {
	/// \brief The index of the point in the array that the ray collided with
	u64   index;

	/// \brief The distance along the ray at which the intersection occurred
	/// IE: intersection point is ray.origin + ray.direction * t
	real  t;
};

bool intersectRayPoints(xen::Ray3r ray,
                        Vec3r* points, u64 point_count,
                        RayPointIntersection& result);

/////////////////////////////////////////////////////////////////////
/// \brief Rasterizes some set of atoms onto the screen
/////////////////////////////////////////////////////////////////////
void rasterizeAtoms(xen::sren::RenderTargetImpl& target,
                    const xen::Aabb2u& viewport,
                    const xen::RenderParameters3d& params,
                    const AtomizerOutput& a_out,
                    const Vec3r* atoms_light
                   );

/////////////////////////////////////////////////////////////////////
/// \brief Raytraces some set of atoms onto the screen
/////////////////////////////////////////////////////////////////////
void raytraceAtoms(xen::sren::RenderTargetImpl& target,
                   const xen::Aabb2u& viewport,
                   const xen::RenderParameters3d& params,
                   const AtomizerOutput& a_out,
                   const Vec3r* atoms_light,
                   const xen::Aabb2u& rendering_bounds);

} // end of namespace sren
} // end of namespace xen

#endif
