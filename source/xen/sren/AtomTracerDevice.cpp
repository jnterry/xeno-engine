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
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>

#include "rasterizer3d.hxx"
#include "SoftwareDeviceBase.hxx"
#include "MeshStore.hxx"

namespace {

	/*inline float _fastInvSqRoot(float number){
	static_assert(sizeof(float) == sizeof(u32), "Expected to be able to store float in u32");
	// Fast inverse square root method, stolen from quake
	u32 i;
	float x2, y;
	x2 = number * 0.5f;
	i  = *(u32*)&number;
	i  = 0x5f3759df - ( i >> 1 );
	y  = *(float*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;
}

inline double _fastInvSqRoot(double number){
	static_assert(sizeof(double) == sizeof(u64), "Expected to be able to store double in u64");
	// Fast inverse square root method, stolen from quake
	u64 i;
	double x2, y;
	x2 = number * 0.5;
	i  = *(u64*)&number;
	i  = 0x5fe6eb50c7b537a9 - ( i >> 1 );
	y  = *(double*)&i;
	y  = y * (1.5f - (x2 * y * y) );
	return y;
	}*/

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


		///////////////////////////////////////////////////////////////////////////
		// Atomize the scene into world space
		Vec4r* cur_pos = (Vec4r*)this->frame_scratch.next_byte;
		for(u32 cmd_index = 0; cmd_index < xen::size(commands); ++cmd_index){
			const xen::RenderCommand3d&      cmd  = commands[cmd_index];
			const xen::sren::RasterizerMesh* mesh = this->mesh_store.getMesh(cmd.mesh);

			switch(cmd.primitive_type){
			case xen::PrimitiveType::POINTS: {
				for(u32 i = 0; i < mesh->vertex_count; ++i){
					*cur_pos = xen::toHomo(mesh->position[i]) * cmd.model_matrix;
					++cur_pos;
				}
				break;
			}
			case xen::PrimitiveType::TRIANGLES: {
				for(u32 i = 0; i < mesh->vertex_count; i += 3){
					Vec4r p0 = xen::toHomo(mesh->position[i+0]) * cmd.model_matrix;
					Vec4r p1 = xen::toHomo(mesh->position[i+1]) * cmd.model_matrix;
					Vec4r p2 = xen::toHomo(mesh->position[i+2]) * cmd.model_matrix;

					Vec4r e1 = p1 - p0;
					Vec4r e2 = p2 - p0;

					// :TODO: is using fastInvSqRoot faster?
					real invDistE1 = (1.0_r / xen::mag(e1)) * 0.01_r;
					real invDistE2 = (1.0_r / xen::mag(e2)) * 0.01_r;

					for(real cur_e1 = 0; cur_e1 <= 1; cur_e1 += invDistE1){
						for(real cur_e2 = 0; cur_e2 <= (1 - cur_e1); cur_e2 += invDistE2){
							*cur_pos = p0 + (cur_e1 * e1) + (cur_e2 * e2);
							++cur_pos;
						}
					}
				}
			}
			case xen::PrimitiveType::LINES:      break;
			case xen::PrimitiveType::LINE_STRIP: break;

			}
		}

		xen::Array<Vec4r> atoms;
		atoms.elements = (Vec4r*)frame_scratch.next_byte;
		atoms.size     = xen::ptrDiff(frame_scratch.next_byte, cur_pos) / sizeof(Vec4r);

		frame_scratch.next_byte = cur_pos;

		//printf("Atomized scene into %li atoms\n", atoms.size);

		///////////////////////////////////////////////////////////////////////////
		// Get camera related data
	  xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
		xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);
		Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

		///////////////////////////////////////////////////////////////////////////
		// Rasterizer the points on screen
		for(u64 atom_index = 0; atom_index < xen::size(atoms); ++atom_index){
			Vec4r point_clip  = atoms[atom_index] * vp_matrix;

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

			target.depth[pixel_index] = point_clip.z;
			target.color[pixel_index] = xen::Color::WHITE4f;
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
