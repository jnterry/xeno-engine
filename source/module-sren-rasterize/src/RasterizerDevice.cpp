////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Rasterizer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZERDEVICE_CPP
#define XEN_SREN_RASTERIZERDEVICE_CPP

#include <xen/sren/RasterizerDevice.hpp>
#include <xen/sren/FragmentShader.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/intrinsics.hpp>

#include <xen/sren/SoftwareDeviceBase.hxx>
#include <xen/sren/render-debug.hxx>
#include <xen/sren/rasterizer3d.hxx>
#include <xen/sren/MeshStore.hxx>

#include "ModuleRasterize.hxx"
#include "Mesh.hxx"

#include <cstring>

void doRasterizerStateInit(void* block, const u64 BLK_SIZE);

class RasterizerDevice : public xen::sren::SoftwareDeviceBase {
public:
	~RasterizerDevice(){
	}

	RasterizerDevice(xen::Array<xsr::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors)
	{
		void* data = malloc(xen::megabytes(4));
	  doRasterizerStateInit(data, xen::megabytes(4));
	}


	xen::Mesh createMesh(const xen::MeshData* mesh_data) override{
		return xsr::createMesh(mesh_data);
	}
	void      destroyMesh         (xen::Mesh mesh) override{
		return xsr::destroyMesh(mesh);
	}
	void      updateMeshVertexData(xen::Mesh mesh,
	                               u32   attrib_index,
	                               void* new_data,
	                               u32   start_vertex,
	                               u32   end_vertex) override{
		return xsr::updateMeshVertexData(mesh,
		                                 attrib_index,
		                                 new_data,
		                                 start_vertex,
		                                 end_vertex
		                                );
	}

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xsr::RenderTarget& target = *this->getRenderTargetImpl(target_handle);

		////////////////////////////////////////////////////////////////////////////
		// Generate view projection matrix
		if(!xen::isCameraValid(params.camera)){
			printf("ERROR: Camera is not valid, skipping rendering\n");
			return;
		}

		// Find the actual view_region we wish to draw to. This is the
		// intersection of the actual target, and the user specified viewport
		xen::Aabb2u screen_rect = { 0, 0, (u32)target.width - 1, (u32)target.height - 1 };
		xen::Aabb2r view_region = (xen::Aabb2r)xen::getIntersection(viewport, screen_rect);

		Mat4r vp_matrix = xen::getViewProjectionMatrix(params.camera, view_region.max - view_region.min);

		if(xen::isnan(vp_matrix)){
			// :TODO: log
			printf("ERROR: vp_matrix contains NaN elements, skipping rendering\n");
			return;
		}
		////////////////////////////////////////////////////////////////////////////

		xsr::RasterizationContext context;
		context.target          = &target;
		context.viewport        = &view_region;

		*(xen::RenderParameters3d*)(&context) = params;
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d& cmd = commands[cmd_index];

			context.fragment_shader = this->getShaderImpl(cmd.shader);
			if(context.fragment_shader == nullptr){
				context.fragment_shader = xsr::FragmentShader_Default;
			}
			setPerCommandFragmentUniforms(context,
			                              (xen::Material&)cmd,
			                              cmd.model_matrix,
			                              vp_matrix
			                             );
			context.textures[0] = this->getTextureImpl(cmd.textures[0]);
			context.textures[1] = this->getTextureImpl(cmd.textures[1]);
			context.textures[2] = this->getTextureImpl(cmd.textures[2]);
			context.textures[3] = this->getTextureImpl(cmd.textures[3]);
			auto mesh = xsr::getMeshImpl(cmd.mesh);

			#if 0
			renderDebugBoundingBox(context,
			                       xen::getTransformed(mesh->bounds, commands[cmd_index].model_matrix)
			                      );
			#endif

			rasterizeMesh(context, cmd.primitive_type, *mesh);
		}
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<xsr::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
