////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Rasterizer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZERDEVICE_CPP
#define XEN_SREN_RASTERIZERDEVICE_CPP

#include <xen/graphics/GraphicDevice.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "renderer3d.hxx"

class RasterizerDevice : public xen::GraphicsDevice {
	~RasterizerDevice(){

	}

	RasterizerDevice(){

	}

	xen::Mesh createMesh(const xen::MeshData& mesh_data){
		// :TODO: implement
	}

	void destroyMesh(xen::Mesh mesh) {
	}

	void clear(xen::RenderTarget target,
	           const xen::Aabb2u& viewport,
	           xen::Color color
	           ) override {
	}

	void render(xen::RenderTarget target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) {
		xen::sren::renderRasterize(target, viewport, params, commands);
	}
};
