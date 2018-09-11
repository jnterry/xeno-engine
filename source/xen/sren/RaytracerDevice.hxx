////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of the RaytracerDevice type
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEVICE_HXX
#define XEN_SREN_RAYTRACERDEVICE_HXX

#include "SoftwareDeviceBase.hxx"
#include "MeshStore.hxx"
#include "raytracer3d.hxx"

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/graphics/GraphicsDevice.hpp>

namespace xen {
namespace sren {

class RaytracerDevice : public xen::sren::SoftwareDeviceBase {
private:
	///< \brief Scratch space for per render call temporary data
	xen::ArenaLinear                         render_scratch_arena;

protected:
	xen::sren::MeshStore<xen::sren::RaytracerMesh> mesh_store;

	/// \brief Called by render after the scene has been generated to actually
	/// fill the target with pixels
	virtual void doRender(xen::sren::RenderTargetImpl&           target,
	                      const xen::Aabb2u&                     viewport,
	                      const xen::RenderParameters3d&         params,
	                      const xen::Array<xen::RenderCommand3d> commands,
	                      const xen::Array<u32>                  non_triangle_cmds,
	                      const RaytracerScene&                  scene);
public:
	~RaytracerDevice();

	RaytracerDevice(xen::Array<xen::sren::PostProcessor*> post_processors);

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override;
	void      destroyMesh         (xen::Mesh mesh) override;
	void      updateMeshVertexData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex) override;

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	           ) override;
}; // end of class RaytracerDevice

} // end of namespace xen
} // end of namespace sren

#endif
