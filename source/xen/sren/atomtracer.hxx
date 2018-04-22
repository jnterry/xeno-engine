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

struct ZOrder {
	enum Values {
		// DO NOT RE-ORDER THESE WITHOUT CHANGING _zorderSplit function
		// -> any maybe other places...
		// Back is defined to be low z
		BACK_TOP_LEFT       = 0,
		BACK_TOP_RIGHT      = 1,
		BACK_BOTTOM_LEFT    = 2,
		BACK_BOTTOM_RIGHT   = 3,

		FRONT_TOP_LEFT      = 4,
		FRONT_TOP_RIGHT     = 5,
		FRONT_BOTTOM_LEFT   = 6,
		FRONT_BOTTOM_RIGHT  = 7,

		END                 = 8,
	};
};

struct ZOrderTreeNode {
	xen::Aabb3r bounds;

	/// \brief The first point in this node of the tree
	Vec3r* start;

	/// \brief The first point past the last point in this node of the tree
	Vec3r* end;

	ZOrderTreeNode* children[8];
};

/// \brief Output data from the scene atomiser
struct AtomScene {
	typedef u32 AtomIndex;
	struct Box {
		/// \brief The bounding box of this box
		xen::Aabb3r bounds;

		/// \brief The index of the first atom in this box
		AtomIndex    start;

		/// \brief The index one past the last atom in this box
		AtomIndex    end;
	};

	/// \brief The locations of atoms in the scene
  Vec3r* positions;

	/// \brief Boxes that divide up the world's atoms spatially
	xen::Array<Box>   boxes;

	/// \brief The full bounding box of the scene
	Aabb3r bounds;

	/// \brief The size of each box in the scene
	Vec3r box_size;

	/// \brief The depth of the splitting of the scene
	u32 split_count;

	AtomIndex atom_count;
};

/////////////////////////////////////////////////////////////////////
/// \brief Atomizes a scene (IE: set of rendering commands) to produce
/// an atom scene.
///
/// \param pixels_per_atom The expected number of pixels a single atom is
/// usually reponsible for. Larger values reduce the number of atoms and
/// hence increase performance, but decrease visual fidelity
/////////////////////////////////////////////////////////////////////
AtomScene& atomizeScene(const xen::Aabb2u& viewport,
                        const xen::RenderParameters3d& params,
                        const xen::Array<xen::RenderCommand3d>& commands,
                        xen::sren::MeshStore<xen::sren::RasterizerMesh>& mesh_store,
                        xen::ArenaLinear& arena,
                        real  pixels_per_atom = 1.0_r);

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
                    const AtomScene& a_out,
                    const Vec3r* atoms_light
                   );

/////////////////////////////////////////////////////////////////////
/// \brief Raytraces some set of atoms onto the screen
/////////////////////////////////////////////////////////////////////
void raytraceAtoms(xen::sren::RenderTargetImpl& target,
                   const xen::Aabb2u& viewport,
                   const xen::RenderParameters3d& params,
                   const AtomScene& a_out,
                   const Vec3r* atoms_light,
                   const xen::Aabb2u& rendering_bounds);

} // end of namespace sren
} // end of namespace xen

#endif
