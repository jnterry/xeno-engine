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
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>
#include <xen/math/quaternion.hpp>

#include <xen/sren/FragmentShader.hpp>
#include "rasterizer3d.hxx"
#include "atomtracer.hxx"
#include "SoftwareDeviceBase.hxx"
#include "MeshStore.hxx"

namespace {

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

		xen::sren::AtomScene& ascene = xen::sren::atomizeScene(viewport, params, commands,
		                                                       mesh_store, frame_scratch,
		                                                       2.0_r
		                                                      );

		///////////////////////////////////////////////////////////////////////////
		// Perform first lighting pass
		xen::Array<Vec3f> atoms_light;
		atoms_light.elements = xen::reserveTypeArray<Vec3f>(frame_scratch, ascene.atom_count);
		atoms_light.size     = ascene.atom_count;

		for(u64 i = 0; i < ascene.atom_count; ++i){
			atoms_light[i] = params.ambient_light;

			for(u64 li = 0; li < xen::size(params.lights); ++li){
				real distance_sq = xen::distanceSq
                    (ascene.positions[i], params.lights[li].point.position);

				atoms_light[i] += xen::sren::computeLightInfluenceSimple
					(params.lights[li].color, params.lights[li].attenuation, distance_sq);
			}
		}

		xen::sren::rasterizeAtoms(target, viewport, params, ascene, atoms_light.elements);
		//raytraceAtoms(target, viewport, params, ascene, atoms_light.elements, viewport);
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
