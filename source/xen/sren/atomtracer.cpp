////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of types and functions declared in atomtracer.hxx
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_ATOMTRACER_CPP
#define XEN_SREN_ATOMTRACER_CPP

#include "atomtracer.hxx"

#include <xen/math/geometry.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

#define XEN_USE_QUAKE_SQROOT 0

namespace {

inline float _fastInvSqRoot(float number){
	#if XEN_USE_QUAKE_INVSQROOT
	static_assert(sizeof(float) == sizeof(u32),
	  "Expected to be able to store float in u32");
	// Fast inverse square root method, stolen from quake
	u32 i;
	float x2, y;
	x2 = number * 0.5f;
	i  = *(u32*)&number;
	i  = 0x5f3759df - ( i >> 1 );
	y  = *(float*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;
	#else
	return 1.0f / sqrt(number);
	#endif
}

inline double _fastInvSqRoot(double number){
	#if XEN_USE_QUAKE_INVSQROOT
	static_assert(sizeof(double) == sizeof(u64),
	              "Expected to be able to store double in u64");
	// Fast inverse square root method, stolen from quake
	u64 i;
	double x2, y;
	x2 = number * 0.5;
	i  = *(u64*)&number;
	i  = 0x5fe6eb50c7b537a9 - ( i >> 1 );
	y  = *(double*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;
	#else
	return 1.0 / sqrt(number);
	#endif
}

Vec3r _convertToScreenSpace(const Vec4r in_clip,
                            const::xen::Aabb2r& viewport){
	Vec3r out_screen = in_clip.xyz;

	// Do perspective divide such that things farther from camera are nearer in
	// screen space
	out_screen.xy /= in_clip.z;

	out_screen.xy += Vec2r{1,1};                  // [-1, 1] => [0, 2] space
	out_screen.xy /= 2.0_r;                       // [ 0, 2] => [0, 1] space
	out_screen.xy *= viewport.max - viewport.min; // [ 0, 1] => screen space (pixels)
	out_screen.xy += viewport.min;                // move to correct part of screen

	return out_screen;
}

xen::sren::AtomScene& _breakSceneIntoAtoms
( const xen::Aabb2u&                               viewport,
  const xen::RenderParameters3d&                   params,
  const xen::Array<xen::RenderCommand3d>&          commands,
  xen::sren::MeshStore<xen::sren::RasterizerMesh>& mesh_store,
  xen::ArenaLinear&                                arena,
  real                                             pixels_per_atom
)
{
	Vec3r cam_pos       = params.camera.position;
	real  tan_fov_per_pixel = xen::tan(params.camera.fov_y / (viewport.max.y - viewport.min.y));

	auto result        = xen::reserveType<xen::sren::AtomScene>(arena);
	result->bounds     = xen::Aabb3r::MaxMinBox;
	result->positions  = (Vec3r*)arena.next_byte;
	Vec3r* cur_pos     = result->positions;

	for(u32 cmd_index = 0; cmd_index < xen::size(commands); ++cmd_index){
		const xen::RenderCommand3d&      cmd  = commands[cmd_index];
		const xen::sren::RasterizerMesh* mesh = mesh_store.getMesh(cmd.mesh);

		xen::Aabb3r mesh_bounds = xen::getTransformed(mesh->bounds, cmd.model_matrix);
		xen::addPoint(result->bounds, mesh_bounds.min);
		xen::addPoint(result->bounds, mesh_bounds.max);

		u32 stride = 2;
		switch(cmd.primitive_type){
		case xen::PrimitiveType::POINTS: {
			for(u32 i = 0; i < mesh->vertex_count; ++i){
				*cur_pos = xen::fromHomo(xen::toHomo(mesh->position[i]) * cmd.model_matrix);
				++cur_pos;
			}
			break;
		}
		case xen::PrimitiveType::TRIANGLES: {
			for(u32 i = 0; i < mesh->vertex_count; i += 3){
				// We want to split the triangle into atoms
				//
				// The nearer the triangle is to the camera the greater the number of
				// atoms we want

				// First compute the world position of the 3 vertices of the triangle
				Vec3r p0 = xen::fromHomo(xen::toHomo(mesh->position[i+0]) * cmd.model_matrix);
				Vec3r p1 = xen::fromHomo(xen::toHomo(mesh->position[i+1]) * cmd.model_matrix);
				Vec3r p2 = xen::fromHomo(xen::toHomo(mesh->position[i+2]) * cmd.model_matrix);

				// We can now compute the distances to each of the triangle's vertices
				//
				// :TODO: this broken if the camera is in the middle of a triangle, eg
				// floating over the flat floor
				// EG:
				//    +-----+
				//    |    /
				//    | . /
				//    |  /
				//    | /
				//    |/
				//    +
				// Camera is at the . in center of triangle, hence the distance to each
				// vertex is some positive value, however near the camera the distance
				// should be basically 0. This leads us to producing too few atoms for
				// the triangle leaving holes when rendering
				//
				// If we let the distance be negative if behind the camera this would
				// work, since we would lerp from -ve to +ve and hence reach 0 distance
				// when right next to the camera...
				real cam_dist_p0 = xen::distance(p0, cam_pos);
				real cam_dist_p1 = xen::distance(p1, cam_pos);
				real cam_dist_p2 = xen::distance(p2, cam_pos);

				// Edge 1 and 2 of the triangle
				Vec3r e1 = p1 - p0;
				Vec3r e2 = p2 - p0;

				// 1d delta "vector" describing how the camera distance
				// varies along edge 1 and edge 2 of the triangle
				real e1_cam_dist = cam_dist_p1 - cam_dist_p0;
				real e2_cam_dist = cam_dist_p2 - cam_dist_p0;

				// Inverse of the length (in world space) of edge 1 and edge 2
				real inv_len_e1 = _fastInvSqRoot(xen::magSq(e1));
				real inv_len_e2 = _fastInvSqRoot(xen::magSq(e2));

				// Delta is how far to step along the triangles edges for each atom
				real delta_e1 = 0;
				real delta_e2 = 0;

				for(real cur_e1 = 0; cur_e1 <= 1; cur_e1 += delta_e1){
					for(real cur_e2 = 0; cur_e2 <= (1 - cur_e1); cur_e2 += delta_e2){
						*cur_pos = p0 + (cur_e1 * e1) + (cur_e2 * e2);

						real cam_dist    = cam_dist_p0 + (cur_e1 * e1_cam_dist) + (cur_e2 * e2_cam_dist);

						// We want our delta between atoms to be roughly equal to the
						// distance in world space between pixels projected onto the
						// surface
						//
						//              distance in world space between pixels = d
						//                             V
						//                          +------+
						//                          |     /
						//                          |    /
						// distance from camera = z |   /
						//                          |  /
						//                          | /
						//                          +
						//               field of view per pixel = a
						//
						// Hence tan(a) = d/z
						// d = z*tan(a)
						real delta = cam_dist * tan_fov_per_pixel;

						// Modify delta by some factor -> higher places less
						// atoms but performs better
						delta *= pixels_per_atom;

						// Work out how far to step along the edge between 0 and 1
						// by doing distance_we_want_to_step / length_of_edge
						// IE: delta * (1/len)
						delta_e1 = delta * inv_len_e1;
						delta_e2 = delta * inv_len_e2;

						++cur_pos;
					}
				}
			}
		}
			break;
		case xen::PrimitiveType::LINE_STRIP: stride = 1;
		case xen::PrimitiveType::LINES:
			for(u32 i = 0; i < mesh->vertex_count; i += stride){
				// Positions of line end points in world space
				Vec3r p0 = xen::fromHomo(xen::toHomo(mesh->position[i+0]) * cmd.model_matrix);
				Vec3r p1 = xen::fromHomo(xen::toHomo(mesh->position[i+1]) * cmd.model_matrix);

				// Distances of line endpoints to the camera in world space
				real cam_dist_p0 = xen::distance(p0, cam_pos);
				real cam_dist_p1 = xen::distance(p1, cam_pos);

				// Edge 1
				Vec3r e1 = p1 - p0;

				// 1d delta "vector" describing how the camera distance
				// varies along edge 1 of the line
				real e1_cam_dist = cam_dist_p1 - cam_dist_p0;

				// Inverse of the length (in world space) of edge 1 and edge 2
				real inv_len_e1 = _fastInvSqRoot(xen::magSq(e1));

				// Delta is how far to step along the triangles edges for each atom
				real delta_e1 = 0;

				for(real cur_e1 = 0; cur_e1 <= 1; cur_e1 += delta_e1){
					*cur_pos = p0 + (cur_e1 * e1);

					real cam_dist    = cam_dist_p0 + (cur_e1 * e1_cam_dist);

					real delta = cam_dist * tan_fov_per_pixel;
					delta *= pixels_per_atom; // This places atoms every n pixels
					delta_e1 = delta * inv_len_e1;

					++cur_pos;
				}
			}
			break;
		}
	}

	result->positions  = (Vec3r*)arena.next_byte;
	result->atom_count = xen::ptrDiff(arena.next_byte, cur_pos) / sizeof(Vec3r);
  arena.next_byte = cur_pos;

	#if 0
  printf("Atomised scene into %li atoms\n", ascene.atoms.size);
  for(u32 i = 0; i < xen::size(ascene.boxes); ++i){
	  printf("  Box %i, bounds: (%8f, %8f, %8f) -> (%8f, %8f, %8f)\n",
	         i,
	         ascene.boxes[i].bounds.min.x,
	         ascene.boxes[i].bounds.min.y,
	         ascene.boxes[i].bounds.min.z,
	         ascene.boxes[i].bounds.max.x,
	         ascene.boxes[i].bounds.max.y,
	         ascene.boxes[i].bounds.max.z
	         );
	  printf("    start: %8lu, end: %8lu\n", ascene.boxes[i].start, ascene.boxes[i].end);
  }
	#endif

	return *result;
}

/////////////////////////////////////////////////////////////////////
/// \brief Splits a set of points on some plane based on a threshold
///
/// \param start The first point to sort
/// \param end   Pointer to vector just past the last point to sort
///
/// \param dimension Which dimension of the points to examine, 0 for x,
/// 1 for y, 2 for z
///
/// \param threshold The threshold value about which the split will occur
/// All those points with the selected dimension of less than the threshold
/// will be placed at the start of the array, all those with less than the
/// selected dimension will be placed at the end
///
/// \return Pointer to first point with the specified dimension
/// being greater than or equal to the threshold
/// Those with dimension <  threshold will run from [start,     returnval - 1]
/// Those with dimension >= threshold will run from [returnval, end       - 1]
/////////////////////////////////////////////////////////////////////
Vec3r* _splitPointsOnPlane(Vec3r* const start, Vec3r* const end,
                           u32 dimension, real threshold){
	Vec3r* cur_front = start;
	Vec3r* cur_back  = end-1;

	Vec3r tmp;
	while(cur_front < cur_back){
		while((*cur_front)[dimension] <  threshold){ ++cur_front; }
		while((*cur_back )[dimension] >= threshold){ --cur_back;  }

		tmp = *cur_front;
		*cur_front = *cur_back;
		*cur_back = tmp;

		++cur_front;
		--cur_back;
	}

	// This is first element >= to the threshold
	return cur_front;
}

struct ZOrderedPoints {
	// We split a bounding box into 8 smaller bounding boxes and use the z-curve
	// ordering to arange the points such that spatially local points are near
	// each other in the linear array
	//
	// The splits array holds a pointer to the first point in each of the 8 blocks
	// It should be assumed that a block runs from splits[n] to splits[n+1]-1
	// The semantic meaning of each split is given by the Splits enum
	Vec3r* splits[9];
};

/////////////////////////////////////////////////////////////////////
/// \brief Sorts a continous list of points into z ordered bins
/// \param start  Pointer to the first point to sort
/// \param end    Pointer just past the last point to sort
/// \param bounds The full bounding box of the points to sort
/////////////////////////////////////////////////////////////////////
ZOrderedPoints _zorderSplit(Vec3r* const start, Vec3r* const end, xen::Aabb3r bounds){
	ZOrderedPoints result;
	result.splits[0] = start;
	result.splits[8] = end;

	Vec3r bound_half = bounds.min + ((bounds.max - bounds.min) / 2.0_r);

	// Split the points into two halves based on their z index
	result.splits[4] = _splitPointsOnPlane(result.splits[0], result.splits[8], 2, bound_half.z);

	// Split each of the z_split groups along y
	result.splits[2] = _splitPointsOnPlane(result.splits[0], result.splits[4], 1, bound_half.y);
	result.splits[6] = _splitPointsOnPlane(result.splits[4], result.splits[8], 1, bound_half.y);

	// Split each of those groups along x
	result.splits[1] = _splitPointsOnPlane(result.splits[0], result.splits[2], 0, bound_half.x);
	result.splits[3] = _splitPointsOnPlane(result.splits[2], result.splits[4], 0, bound_half.x);
	result.splits[5] = _splitPointsOnPlane(result.splits[4], result.splits[6], 0, bound_half.x);
	result.splits[7] = _splitPointsOnPlane(result.splits[6], result.splits[8], 0, bound_half.x);

	return result;
}

} // end of anon namespace

void _splitZOrderTreeNode(xen::ArenaLinear& node_arena,
                          xen::sren::ZOrderTreeNode*  node){

	ZOrderedPoints zordered = _zorderSplit(node->start, node->end, node->bounds);

	for(u32 i = 0; i < 8; ++i){
		node->children[i] = xen::reserveType<xen::sren::ZOrderTreeNode>(node_arena);

		node->children[i]->start = zordered.splits[i+0];
		node->children[i]->end   = zordered.splits[i+1];
	}

	Vec3r b_a = node->bounds.min;
	Vec3r b_b = node->bounds.min;
	Vec3r b_c = b_a + ((b_c - b_a) / 2.0_r);

	xen::sren::ZOrderTreeNode** cs = node->children;

	// For back nodes, z is always a -> b
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.min.z = b_a.z;
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.max.z = b_b.z;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.min.z = b_a.z;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.max.z = b_b.z;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.min.z = b_a.z;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.max.z = b_b.z;
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.min.z = b_a.z;
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.max.z = b_b.z;

	// For front nodes, z is always b -> c
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.min.z = b_b.z;
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.max.z = b_c.z;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.min.z = b_b.z;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.max.z = b_c.z;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.min.z = b_b.z;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.max.z = b_c.z;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.min.z = b_b.z;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.max.z = b_c.z;

	// For bottom nodes y is always a -> b
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.min.y = b_a.y;
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.max.y = b_b.y;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.min.y = b_a.y;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.max.y = b_b.y;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.min.y = b_a.y;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.max.y = b_b.y;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.min.y = b_a.y;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.max.y = b_b.y;

	// For top nodes y is always b -> c
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.min.y = b_b.y;
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.max.y = b_c.y;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.min.y = b_b.y;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.max.y = b_c.y;
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.min.y = b_b.y;
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.max.y = b_c.y;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.min.y = b_b.y;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.max.y = b_c.y;

	// For right nodes x is always b -> c
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.max.x = b_b.x;

	// For left nodes x is always a -> b
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_TOP_LEFT      ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_LEFT   ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_TOP_LEFT     ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_LEFT  ]->bounds.max.x = b_b.x;

	// For right nodes x is always b -> c
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_TOP_RIGHT     ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::BACK_BOTTOM_RIGHT  ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_TOP_RIGHT    ]->bounds.max.x = b_b.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.min.x = b_a.x;
	cs[xen::sren::ZOrder::FRONT_BOTTOM_RIGHT ]->bounds.max.x = b_b.x;
}

namespace xen {
namespace sren {

AtomScene& atomizeScene(const Aabb2u& viewport,
                        const RenderParameters3d& params,
                        const Array<RenderCommand3d>& commands,
                        MeshStore<RasterizerMesh>& mesh_store,
                        ArenaLinear& arena,
                        real pixels_per_atom){

	AtomScene& result = _breakSceneIntoAtoms(viewport, params, commands,
	                                         mesh_store, arena,
	                                         pixels_per_atom);

	ZOrderTreeNode* root = xen::reserveType<ZOrderTreeNode>(arena);

	root->bounds = result.bounds;
	root->start  = &result.positions[0                  ];
	root->end    = &result.positions[result.atom_count-1];

    _splitZOrderTreeNode(arena, root);

	return result;
}

bool intersectRayPoints(xen::Ray3r ray,
                        Vec3r* points, u64 point_count,
                        RayPointIntersection& result){

	const real sphere_radius    = 0.01_r;
	const real sphere_radius_sq = sphere_radius * sphere_radius;

	result.t = xen::RealMax;

	for(u64 i = 0; i < point_count; ++i){
		// https://gamedev.stackexchange.com/a/96487

		Vec3r m = ray.origin - points[i];
		real  b = xen::dot(m, ray.direction);
		real  c = xen::dot(m, m) - sphere_radius_sq;

		if(c > 0.0f && b > 0.0f){ continue; }

		// We have effectively combined equation for sphere and for ray,
		// yielding quadratic, if discriminant is < 0 then no solutions
		// so bail out
		real discriminant = b*b - c;
		if(discriminant < 0.0f){ continue; }

		real t = -b - xen::sqrt(discriminant);

		// If t less than 0 then ray started inside sphere, clamp to 0
		if(t <= 0.0_r){
			result.t           = 0.0_r;
			result.index = i;
			return true; // we can't get closer than 0
		}

		if(t < result.t){
			result.t           = t;
			result.index = i;
		}
	}

	return result.t < xen::RealMax;
}


/////////////////////////////////////////////////////////////////////
/// \brief Rasterizes some set of atoms onto the screen
/////////////////////////////////////////////////////////////////////
void rasterizeAtoms(xen::sren::RenderTargetImpl& target,
                    const xen::Aabb2u& viewport,
                    const xen::RenderParameters3d& params,
                    const AtomScene& ascene,
                    const Vec3r* atoms_light
                   ){
	///////////////////////////////////////////////////////////////////////////
	// Get camera related data
	xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
	xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
	Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

	///////////////////////////////////////////////////////////////////////////
	// Rasterizer the points on screen
	for(u32 atom_index = 0; atom_index < ascene.atom_count; ++atom_index){
		Vec4r point_clip  = xen::toHomo(ascene.positions[atom_index]) * vp_matrix;

		if(point_clip.x <= -point_clip.w ||
		   point_clip.x >=  point_clip.w ||
		   point_clip.y <= -point_clip.w ||
		   point_clip.y >=  point_clip.w ||
		   point_clip.z <= -point_clip.w ||
		   point_clip.z >=  point_clip.w){
			// Then point is not in view of the camera
			continue;
		}

		Vec3r point_screen = _convertToScreenSpace(point_clip, view_region);

		//for(s32 dy = -2; dy <= 2; ++dy){
		//	for(s32 dx = -2; dx <= 2; ++dx){
		for(s32 dy = -1; dy <= 1; ++dy){
			for(s32 dx = -1; dx <= 1; ++dx){
				if(point_screen.x + dx < 0 || point_screen.x + dx > target.width ||
				   point_screen.y + dy < 0 || point_screen.y + dy > target.width
				   ){
					continue;
				}

				u32 pixel_index = ((u32)(point_screen.y + dy) * target.width +
				                   (u32)(point_screen.x + dx)
				                  );

				if (point_screen.z > target.depth[pixel_index]){
					// Then point is behind something else occupying this pixel
					continue;
				}

				target.depth[pixel_index]     = point_clip.z;
				target.color[pixel_index]     = xen::Color::WHITE4f;
			}
		}
	}
}

void raytraceAtoms(xen::sren::RenderTargetImpl& target,
                   const xen::Aabb2u& viewport,
                   const xen::RenderParameters3d& params,
                   const AtomScene& ascene,
                   const Vec3r* atoms_light,
                   const xen::Aabb2u& rendering_bounds){

	xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
	xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
	Vec2s       target_size = (Vec2s)xen::getSize(view_region);

	xen::Angle fov_y = params.camera.fov_y;
	xen::Angle fov_x = params.camera.fov_y * ((real)target_size.y / (real)target_size.x);

	// Compute the local axes of the camera
	Vec3r cam_zaxis = params.camera.look_dir;
	Vec3r cam_xaxis = xen::cross(params.camera.look_dir, params.camera.up_dir);
	Vec3r cam_yaxis = xen::cross(cam_xaxis, cam_zaxis);

	// Compute distance between pixels on the image plane in world space using
	// a bit of trig
	//            xm
	//         _______
	//         |     /
	//         |    /
	//  z_near |   /
	//         |  /
	//         | /
	//         |/ angle = fov_x / target_width
	//
	// In a typical scene the camera is usually at a +ve z position looking in
	// the -ve z direction. In camera space however we assume that the camera is
	// looking down its own local z axis, IE, in a +ve z direction. Flipping 1
	// axis without flipping others is a change from right-handed world space
	// top left-handed camera space, hence the - in front of x but not y
	//
	// see: notes.terry.cloud/cs/graphics/handedness-cameras-and-mirrors
	// for details
	Vec3r image_plane_center = (params.camera.position +
	                            cam_zaxis * params.camera.z_near
	                           );

	Vec3r image_plane_pixel_offset_x = -(xen::normalized(cam_xaxis) *
	                                     xen::tan(fov_x / (real)target_size.x) * params.camera.z_near
	                                     );
	Vec3r image_plane_pixel_offset_y = (xen::normalized(cam_yaxis) *
	                                    xen::tan(fov_y / (real)target_size.y) * params.camera.z_near
	                                    );

	//////////////////////////////////////////////////////////////////////////
	// Loop over all pixels
	Vec2u target_pos;
	RayPointIntersection intersection = {};
	for(target_pos.x = rendering_bounds.min.x; target_pos.x < rendering_bounds.max.x; ++target_pos.x) {
		for(target_pos.y = rendering_bounds.min.y; target_pos.y < rendering_bounds.max.y; ++target_pos.y) {
			/////////////////////////////////////////////////////////////////////
			// Compute where the ray would intersect the image plane
			Vec2r center_offset = ((Vec2r)target_size / 2.0_r) - (Vec2r)target_pos;
			Vec3r image_plane_position =
				image_plane_center +
				center_offset.x * image_plane_pixel_offset_x +
				center_offset.y * image_plane_pixel_offset_y;

			/////////////////////////////////////////////////////////////////////
			// Construct the primary ray
			xen::Ray3r primary_ray;
			primary_ray.origin    = image_plane_position;;
			primary_ray.direction = xen::normalized(image_plane_position - params.camera.position);

			if(!intersectRayPoints(primary_ray,
			                       ascene.positions, ascene.atom_count,
			                       intersection
			                      )){
				continue;
			}

			xen::Color4f pixel_color;
			pixel_color.rgb = atoms_light[intersection.index];
			pixel_color.a   = 1.0f;
			Vec2u pixel_coord = target_pos + (Vec2u)view_region.min;
			target.color[pixel_coord.y*target.width + pixel_coord.x] = pixel_color;
		}
	}
}

} // end of namespace sren
} // end of namespace xen
#endif
