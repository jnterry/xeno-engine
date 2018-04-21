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
#include <xen/math/angle.hpp>

#include <xen/sren/FragmentShader.hpp>
#include "render-utilities.hxx"
#include "rasterizer3d.hxx"
#include "atomtracer.hxx"
#include "SoftwareDeviceBase.hxx"
#include "MeshStore.hxx"

namespace {

class AtomTracerDebugDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::sren::MeshStore<xen::sren::RasterizerMesh> mesh_store;

	xen::ArenaLinear frame_scratch;

	xen::Stopwatch stopwatch;

public:
	~AtomTracerDebugDevice(){
		this->mesh_store.destroyAllMeshes();
	}

	AtomTracerDebugDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
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

		xen::RenderParameters3d test_params = params;

		xen::Angle cycle = xen::asSeconds<real>(stopwatch.getElapsedTime()) * 60_deg;

		real cam_dist = ((xen::sin(cycle * 0.43_r) + 1.0_r) * 2.0_r) + 1.5_r;
		test_params.camera.position   = xen::rotated (Vec3r{cam_dist, 0, 0}, Vec3r::UnitY, cycle);
		test_params.camera.position.y = xen::sin(cycle * 0.6_r);

		test_params.camera.look_dir = xen::normalized(-test_params.camera.position);

		xen::sren::AtomizerOutput& a_out = xen::sren::atomizeScene(viewport, test_params, commands,
		                                                           mesh_store, frame_scratch
		                                                           );

		///////////////////////////////////////////////////////////////////////////
		// Perform first lighting pass
		xen::Array<Vec3f> atoms_light;
		atoms_light.elements = xen::reserveTypeArray<Vec3f>(frame_scratch, a_out.atoms.size);
		atoms_light.size     = a_out.atoms.size;
		for(u64 i = 0; i < xen::size(a_out.atoms); ++i){
			atoms_light[i] = xen::Color::WHITE4f.rgb;
		}

		xen::sren::rasterizeAtoms(target, viewport, params, a_out, atoms_light.elements);

		xen::sren::renderCameraDebug(target, viewport,
		                             params.camera, test_params.camera,
		                             2
		                            );
	}
};

} // end of anon namespace

namespace xen {
	GraphicsDevice* createAtomTracerDebugDevice(ArenaLinear& arena,
	                                            xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<AtomTracerDebugDevice>(arena, post_processors);
	}
}

#endif
