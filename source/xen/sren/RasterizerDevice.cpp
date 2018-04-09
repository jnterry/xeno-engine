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

		if(attrib_nor != xen::MeshData::BAD_ATTRIB_INDEX){
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
			memcpy(mesh_geom->position, mesh_data->attrib_data[attrib_pos], byte_count_nor);
		}

		if(attrib_col != xen::MeshData::BAD_ATTRIB_INDEX){
			// :TODO: support other color formats
			XenAssert((mesh_data->attrib_types[attrib_nor] &
			           xen::VertexAttribute::_TypeMask
			           ) == xen::VertexAttribute::_TypeReal,
			          "Expected color components to be floats"
			         );
			XenAssert((mesh_data->attrib_types[attrib_nor] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 3,
			          "Expected color attribute to have 3 channels"
			         );

			u32 byte_count_color  = sizeof(xen::Color) * mesh_geom->vertex_count;
			mesh_geom->color      = (xen::Color*)this->mesh_allocator->allocate(byte_count_color);

			xen::Color3f* src_buf = (xen::Color3f*)mesh_data->attrib_data[attrib_col];

			for(u32 i = 0; i < mesh_geom->vertex_count; ++i){
				mesh_geom->color[i] = xen::Color(src_buf[i]);
			}
		}

		return this->makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);
	}

	void destroyMesh(xen::Mesh mesh) override {
		// :TODO: implement - resource leak
	}

	void render(xen::RenderTarget target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) override {
		xen::sren::renderRasterize(*this->getRenderTargetImpl(target), viewport, params, commands);
	}
};

namespace xen {
	GraphicsDevice* createRasterizerDevice(ArenaLinear& arena,
	                                       xen::Array<sren::PostProcessor*> post_processors){
		return xen::emplace<RasterizerDevice>(arena, post_processors);
	}
}

#endif
