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

#include <cstring>

class RasterizerDevice : public xen::sren::SoftwareDeviceBase {
private:
	xen::Allocator*                         mesh_allocator;
	xen::ArenaPool<xen::MeshGeometrySource> mesh_pool;
public:
	~RasterizerDevice(){

	}

	RasterizerDevice(xen::Array<xen::sren::PostProcessor*> post_processors)
		: SoftwareDeviceBase(post_processors),
		  mesh_allocator(main_allocator),
		  mesh_pool(xen::createArenaPool<xen::MeshGeometrySource>(main_allocator, 1024))
	{
		// no-op
	}

	xen::Mesh createMesh(const xen::MeshData* mesh_data) override {
		// :TODO:COMP:ISSUE_31: object pool with automatic handles / resizeable pool
		u32 slot = xen::reserveSlot(this->mesh_pool);
		xen::MeshGeometrySource* mesh_geom = &this->mesh_pool.slots[slot].item;

		// Allocate storage and copy over attributes, this is equivalent
		// to uploading to the gpu in a gl device
		mesh_geom->vertex_count = mesh_data->vertex_count;

		u08 attrib_pos = xen::findMeshAttrib(mesh_data,
		                                     xen::VertexAttribute::_AspectPosition
		                                    );
		u08 attrib_nor = xen::findMeshAttrib(mesh_data,
		                                     xen::VertexAttribute::_AspectNormal
		                                    );
		u08 attrib_col = xen::findMeshAttrib(mesh_data,
		                                     xen::VertexAttribute::_AspectColor
		                                    );

		XenAssert(attrib_pos != xen::MeshData::BAD_ATTRIB_INDEX,
		          "Mesh must have position attribute");
		XenAssert(mesh_data->attrib_data[attrib_pos] != nullptr,
		          "Data source for position attribute must be non-null"
		         );
		{
			XenAssert((mesh_data->attrib_types[attrib_pos] &
			           xen::VertexAttribute::_TypeMask
			           ) == xen::VertexAttribute::_TypeReal,
			          "Expected position components to be reals"
			          );
			XenAssert((mesh_data->attrib_types[attrib_pos] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 3,
			          "Expected position attribute to have 3 channels"
			          );
			u32 byte_count_pos  = sizeof(Vec3r) * mesh_geom->vertex_count;
			mesh_geom->position = (Vec3r*)this->mesh_allocator->allocate(byte_count_pos);
			memcpy(mesh_geom->position, mesh_data->attrib_data[attrib_pos], byte_count_pos);
		}

		if(attrib_nor != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh_data->attrib_data[attrib_nor] != nullptr
		  ){
			XenAssert((mesh_data->attrib_types[attrib_nor] &
			           xen::VertexAttribute::_TypeMask
			          ) == xen::VertexAttribute::_TypeReal,
			          "Expected normal components to be reals"
			         );
			XenAssert((mesh_data->attrib_types[attrib_nor] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 3,
			          "Expected normal attribute to have 3 channels"
			         );
			u32 byte_count_nor  = sizeof(Vec3r) * mesh_geom->vertex_count;
			mesh_geom->normal   = (Vec3r*)this->mesh_allocator->allocate(byte_count_nor);
			memcpy(mesh_geom->normal, mesh_data->attrib_data[attrib_nor], byte_count_nor);
		}

		if(attrib_col != xen::MeshData::BAD_ATTRIB_INDEX &&
		    mesh_data->attrib_data[attrib_col] != nullptr
		  ){
			u32 byte_count_color  = sizeof(xen::Color) * mesh_geom->vertex_count;
			mesh_geom->color      = (xen::Color*)this->mesh_allocator->allocate(byte_count_color);

			switch(mesh_data->attrib_types[attrib_col]){
			case xen::VertexAttribute::Color3f: {
				xen::Color3f* src_buf = (xen::Color3f*)mesh_data->attrib_data[attrib_col];
				for(u32 i = 0; i < mesh_geom->vertex_count; ++i){
					mesh_geom->color[i] = xen::Color(src_buf[i]);
				}
				break;
			}
			case xen::VertexAttribute::Color4b: {
				memcpy(mesh_geom->color, mesh_data->attrib_data[attrib_col], byte_count_color);
				break;
			}
			default:
				XenInvalidCodePath("Found bad color format in mesh");
			}
		}

		return this->makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);
	}

	void destroyMesh(xen::Mesh mesh) override {
		// :TODO: implement - resource leak
	}

	void render(xen::RenderTarget target_handle,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::sren::RenderTargetImpl& target = *this->getRenderTargetImpl(target_handle);

		//////////////////////////////////////////////////////////////////////////////
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
		//////////////////////////////////////////////////////////////////////////////

		const xen::RenderCommand3d* cmd;
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			cmd = &commands[cmd_index];

			xen::Color4f base_color = cmd->color;
			base_color.rgb *= params.ambient_light;

			/////////////////////////////////////////////////////////////////
			// Determine the source for the geometry to draw based on the cmd
			const xen::MeshGeometrySource* geom = nullptr;
			switch(cmd->geometry_source){
			case xen::RenderCommand3d::IMMEDIATE:
				geom = &cmd->immediate;
				break;
			case xen::RenderCommand3d::MESH:
				geom = &this->mesh_pool.slots[cmd->mesh._id].item;
				break;
			}

			/////////////////////////////////////////////////////////////////
			// Do the drawing, based on primitive type
			switch(cmd->primitive_type){
			case xen::PrimitiveType::POINTS:
				rasterizePointsModel(target, view_region, params,
				                     cmd->model_matrix, vp_matrix, cmd->color,
				                     geom->position,
				                     geom->color,
				                     geom->vertex_count);
				break;
			case xen::PrimitiveType::LINES:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    geom->position,
				                    geom->color,
				                    geom->vertex_count,
				                    2); //advance by 2 vertices for each line drawn
				break;
			case xen::PrimitiveType::LINE_STRIP:
				rasterizeLinesModel(target, view_region, params,
				                    cmd->model_matrix, vp_matrix, cmd->color,
				                    geom->position,
				                    geom->color,
				                    geom->vertex_count,
				                    1); //advance by 1 vertex for each line drawn
				break;
			case xen::PrimitiveType::TRIANGLES: {
				rasterizeTrianglesModel(target, view_region, params,
				                        cmd->model_matrix, vp_matrix, cmd->color,
				                        geom->position,
				                        geom->normal,
				                        geom->color,
				                        geom->vertex_count);
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
