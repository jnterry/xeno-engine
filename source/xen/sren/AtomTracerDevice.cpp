////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software AtomTracer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_ATOMTRACERDEVICE_CPP
#define XEN_SREN_ATOMTRACERDEVICE_CPP

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/math/geometry.hpp>
#include <xen/math/vector.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>

#include <xen/sren/FragmentShader.hpp>
#include "rasterizer3d.hxx"
#include "SoftwareDeviceBase.hxx"
#include "MeshStore.hxx"

namespace {


inline float _fastInvSqRoot(float number){
	/*static_assert(sizeof(float) == sizeof(u32), "Expected to be able to store float in u32");
	// Fast inverse square root method, stolen from quake
	u32 i;
	float x2, y;
	x2 = number * 0.5f;
	i  = *(u32*)&number;
	i  = 0x5f3759df - ( i >> 1 );
	y  = *(float*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;*/
	return 1.0f / sqrt(number);
}

inline double _fastInvSqRoot(double number){
	static_assert(sizeof(double) == sizeof(u64), "Expected to be able to store double in u64");
	// Fast inverse square root method, stolen from quake
	/*u64 i;
	double x2, y;
	x2 = number * 0.5;
	i  = *(u64*)&number;
	i  = 0x5fe6eb50c7b537a9 - ( i >> 1 );
	y  = *(double*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;*/
	return 1.0 / sqrt(number);
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

/// \brief Output data from the scene atomiser
struct AtomizerOutput {
	struct Box {
		/// \brief The bounding box of this box
		xen::Aabb3r bounds;

		/// \brief The index of the first atom in this box
		u64    first;

		/// \brief The index of the last atom in this box
		u64    last;
	};

	/// \brief The locations of atoms in the scene
	xen::Array<Vec3r> atoms;
};

AtomizerOutput& atomizeScene(const xen::Aabb2u& viewport,
                             const xen::RenderParameters3d& params,
                             const xen::Array<xen::RenderCommand3d>& commands,
                             xen::sren::MeshStore<xen::sren::RasterizerMesh>& mesh_store,
                             xen::ArenaLinear& arena){

	AtomizerOutput* result = xen::reserveType<AtomizerOutput>(arena);

	Vec3r cam_pos      = params.camera.position;
	real fov_per_pixel = params.camera.fov_y.radians / (viewport.max.y - viewport.min.y);
	Vec3r* cur_pos     = (Vec3r*)arena.next_byte;

	for(u32 cmd_index = 0; cmd_index < xen::size(commands); ++cmd_index){
		const xen::RenderCommand3d&      cmd  = commands[cmd_index];
		const xen::sren::RasterizerMesh* mesh = mesh_store.getMesh(cmd.mesh);

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
				real deltaE1 = inv_len_e1;
				real deltaE2 = inv_len_e2;

				for(real cur_e1 = 0; cur_e1 <= 1; cur_e1 += deltaE1){
					for(real cur_e2 = 0; cur_e2 <= (1 - cur_e1); cur_e2 += deltaE2){

						*cur_pos = p0 + (cur_e1 * e1) + (cur_e2 * e2);

						real cam_dist = cam_dist_p0 + (cur_e1 * e1_cam_dist) + (cur_e2 * e2_cam_dist);

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
						// Using small angle approximation, d = z * a
						real delta = (cam_dist * fov_per_pixel);

						// We can adjust quality by increasing delta by some amount
						delta *= 5; // This places atoms every 3 pixels

						// Work out how far to step along the edge between 0 and 1
						// by doing distance_we_want_to_step / length_of_edge
						// IE: delta * (1/len)
						deltaE1 = delta * inv_len_e1;
						deltaE2 = delta * inv_len_e2;

						++cur_pos;
					}
				}
			}
		}
			break;
		case xen::PrimitiveType::LINE_STRIP: stride = 1;
		case xen::PrimitiveType::LINES:
			for(u32 i = 0; i < mesh->vertex_count; i += stride){
				Vec3r p0 = xen::fromHomo(xen::toHomo(mesh->position[i+0]) * cmd.model_matrix);
				Vec3r p1 = xen::fromHomo(xen::toHomo(mesh->position[i+1]) * cmd.model_matrix);

				Vec3r e1 = p1 - p0;

				real invDistE1 = (1.0_r / xen::mag(e1)) * 0.02_r;

				for(real cur_e1 = 0; cur_e1 <= 1; cur_e1 += invDistE1){
					*cur_pos = p0 + (cur_e1 * e1);
					++cur_pos;
				}
			}
			break;
		}
	}

	result->atoms.elements = (Vec3r*)arena.next_byte;
	result->atoms.size     = xen::ptrDiff(arena.next_byte, cur_pos) / sizeof(Vec3r);
  arena.next_byte = cur_pos;

	return *result;
}

class AtomTracerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::sren::MeshStore<xen::sren::RasterizerMesh> mesh_store;

	xen::ArenaLinear frame_scratch;

public:
	~AtomTracerDevice(){
		this->mesh_store.destroyAllMeshes();
	}

	AtomTracerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		  mesh_store(this, main_allocator),
		  frame_scratch(xen::createArenaLinear(*main_allocator, xen::megabytes(128)))
	{
		// no-op
	}


	xen::Mesh createMesh(const xen::MeshData* mesh_data) override{
		return this->mesh_store.createMesh(mesh_data);
	}
	void      destroyMesh         (xen::Mesh mesh) override{
		this->mesh_store.destroyMesh(mesh);
	}
	void      updateMeshAttribData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex) override{
		this->mesh_store.updateMeshAttribData(mesh,
		                                      attrib_index,
		                                      new_data,
		                                      start_vertex,
		                                      end_vertex);
	}

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::sren::RenderTargetImpl& target = *this->getRenderTargetImpl(target_handle);

		xen::MemoryTransaction transaction(this->frame_scratch);

		AtomizerOutput& a_out = atomizeScene(viewport, params, commands,
		                                     mesh_store, frame_scratch
		                                    );

		printf("Atomised scene into %li atoms\n", a_out.atoms.size);

		///////////////////////////////////////////////////////////////////////////
		// Perform first lighting pass
		xen::Array<Vec3f> atoms_light;
		atoms_light.elements = xen::reserveTypeArray<Vec3f>(frame_scratch, a_out.atoms.size);
		atoms_light.size     = a_out.atoms.size;

		for(u64 i = 0; i < xen::size(a_out.atoms); ++i){
			atoms_light[i] = params.ambient_light;

            for(u64 li = 0; li < xen::size(params.lights); ++li){
				real distance_sq = xen::distanceSq
                    (a_out.atoms[i], params.lights[li].point.position);

				atoms_light[i] += xen::sren::computeLightInfluenceSimple
					(params.lights[li].color, params.lights[li].attenuation, distance_sq);
			}
		}

		///////////////////////////////////////////////////////////////////////////
		// Get camera related data
	  xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
		xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
		Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

		///////////////////////////////////////////////////////////////////////////
		// Rasterizer the points on screen
		for(u64 atom_index = 0; atom_index < xen::size(a_out.atoms); ++atom_index){
			Vec4r point_clip  = xen::toHomo(a_out.atoms[atom_index]) * vp_matrix;

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

			if(point_screen.x < 0 || point_screen.x > target.width ||
			   point_screen.y < 0 || point_screen.y > target.height){
				continue;
			}

			u32 pixel_index = (u32)point_screen.y * target.width + (u32)point_screen.x;

			if (point_screen.z > target.depth[pixel_index]){
				// Then point is behind something else occupying this pixel
				continue;
			}

			target.depth[pixel_index]     = point_clip.z;
			target.color[pixel_index].rgb = atoms_light[atom_index];//xen::Color::WHITE4f;
		}
	}
};

} // end of anon namespace

namespace xen {
	GraphicsDevice* createAtomTracerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<AtomTracerDevice>(arena, post_processors);
	}
}

#endif
