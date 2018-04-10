////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the software Rasterizer graphics device
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_RASTERIZERDEVICE_CPP
#define XEN_SREN_RASTERIZERDEVICE_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/sren/SoftwareDevice.hpp>
#include <xen/math/geometry.hpp>

#include "SoftwareDeviceBase.hxx"
#include "render-utilities.hxx"
#include "rasterizer3d.hxx"
#include "MeshStore.hxx"

#include <cstring>

class RasterizerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::sren::MeshStore<xen::sren::RasterizerMesh> mesh_store;
public:
	~RasterizerDevice(){
		this->mesh_store.destroyAllMeshes();
	}

	RasterizerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		  mesh_store(this, main_allocator)
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

		const xen::RenderCommand3d* cmd;
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			cmd = &commands[cmd_index];

			xen::Color4f base_color = cmd->color;
			base_color.rgb *= params.ambient_light;

			const xen::sren::RasterizerMesh* mesh = this->mesh_store.getMesh(cmd->mesh);

			/////////////////////////////////////////////////////////////////
			// Do the drawing, based on primitive type
			switch(cmd->primitive_type){
			case xen::PrimitiveType::POINTS:
				rasterizePointsModel(target, view_region, params,
				                     cmd->model_matrix, vp_matrix, cmd->color,
				                     mesh->position,
				                     mesh->color,
				                     mesh->vertex_count);
				break;
			case xen::PrimitiveType::LINES:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    mesh->position,
				                    mesh->color,
				                    mesh->vertex_count,
				                    2); //advance by 2 vertices for each line drawn
				break;
			case xen::PrimitiveType::LINE_STRIP:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    mesh->position,
				                    mesh->color,
				                    mesh->vertex_count,
				                    1); //advance by 1 vertex for each line drawn
				break;
			case xen::PrimitiveType::TRIANGLES: {
				rasterizeTrianglesModel(target, view_region, params,
				                        cmd->model_matrix, vp_matrix, cmd->color,
				                        mesh->position,
				                        mesh->normal,
				                        mesh->color,
				                        mesh->vertex_count);
				break;
			}
			default:
				XenInvalidCodePath("Unhandled render command type in rasterizer device");
				break;
			}
		}
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
