////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Raytracer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RAYTRACERDEVICE_CPP
#define XEN_SREN_RAYTRACERDEVICE_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/sren/SoftwareDevice.hpp>

#include "SoftwareDeviceBase.hxx"
#include "renderer3d.hxx"


class RaytracerDevice : public xen::sren::SoftwareDeviceBase {
public:
	~RaytracerDevice(){

	}

	RaytracerDevice(xen::Array<xen::sren::PostProcessor> post_processors)
		: SoftwareDeviceBase(post_processors) {
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData& mesh_data) override {
	  return xen::makeNullHandle<xen::Mesh>();
		// :TODO: implement
		return xen::makeNullHandle<xen::Mesh>();
	}

	void destroyMesh(xen::Mesh mesh) override {
		// :TODO: implement
	}

	void render(xen::RenderTarget target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::sren::renderRaytrace(*this->getRenderTargetImpl(target), viewport, params, commands);
	}
};

namespace xen {
	GraphicsDevice* createRaytracerDevice(ArenaLinear& arena,
	                                      xen::Array<sren::PostProcessor> post_processors){
		return xen::emplace<RaytracerDevice>(arena, post_processors);
	}
}

#endif
