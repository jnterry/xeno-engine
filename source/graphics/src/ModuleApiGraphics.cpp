////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API agnositc implementation of GraphicsModuleApi
/// related functions
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSMODULEAPI_CPP
#define XEN_GRAPHICS_GRAPHICSMODULEAPI_CPP

#include <xen/graphics/ModuleApiGraphics.hpp>
#include <xen/graphics/Image.hpp>
#include <xen/math/geometry.hpp>
#include <xen/core/intrinsics.hpp>
#include <xen/core/array.hpp>

#include <cstdarg>

xen::RenderOp xen::RenderOp::Clear(xen::RenderTarget target, xen::Color color){
	xen::RenderOp result;
	result.type         = xen::RenderOp::CLEAR;
	result.clear.target = target;
	result.clear.color  = color;
	return result;
}

xen::RenderOp xen::RenderOp::Draw(xen::RenderTarget target, xen::Aabb2u viewport,
                    xen::RenderParameters3d& params, xen::Array<RenderCommand3d>& commands
                   ){
	xen::RenderOp result;
	result.type          = xen::RenderOp::DRAW;
	result.draw.target   = target;
	result.draw.viewport = viewport;
	result.draw.params   = &params;
	result.draw.commands = commands;
	return result;
}

xen::RenderOp xen::RenderOp::SwapBuffers(xen::RenderTarget target){
	xen::RenderOp result;
	result.type                = xen::RenderOp::SWAP_BUFFERS;
	result.swap_buffers.target = target;
	return result;
}

const xen::Mesh* xen::ModuleApiGraphics::createMesh(const xen::VertexSpec& vertex_spec,
                                                    const xen::PrimitiveType primitive_type,
                                                    u32 vertex_count,
                                                    ...){

	void* attrib_data[255]; // Can only have up to 255 attributes in mesh

	xen::MeshData md;

	md.vertex_spec    = vertex_spec;
	md.primitive_type = primitive_type;
	md.vertex_count   = vertex_count;
	md.vertex_data    = attrib_data;

	Vec3r* pbuf = nullptr;

	va_list args;
	va_start(args, vertex_count);
	for(u32 i = 0; i < xen::size(vertex_spec); ++i){
		attrib_data[i] = va_arg(args, void*);
		if(vertex_spec[i] == xen::VertexAttribute::Position3r){
			pbuf = (Vec3r*)attrib_data[i];
		}
	}
	va_end(args);

	XenAssert(pbuf != nullptr, "Expected mesh to have position data");

	md.bounds = xen::computeBoundingBox(pbuf, vertex_count);

	/////////////////////////////////////////////////////////////////

	return this->createMesh(&md);
}


const xen::Texture* xen::ModuleApiGraphics::createTexture(const xen::RawImage* image){
	const void* data[1] = { image->pixels };
	return this->_createTexture(xen::Texture::Plane,
	                            false, 4, // 4 channel bytes
	                            Vec3u{image->size.x, image->size.y, 1},
	                            data);
}

const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::RawImage images[6]){
	const void* data[6] = { images[0].pixels, images[1].pixels, images[2].pixels,
	                        images[3].pixels, images[4].pixels, images[5].pixels };

	return this->_createTexture(xen::Texture::CubeMap,
	                            false, 4, // 4 channel bytes
	                            Vec3u{images[0].size.x, images[0].size.y, 6},
	                            data);
}

const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::CubeArray<xen::Color>& data){
	u64 step = data.side_length * data.side_length;
	const void* slices[6] = {
		&data.elements[step * 0], &data.elements[step * 1], &data.elements[step * 2],
		&data.elements[step * 3], &data.elements[step * 4], &data.elements[step * 5],
	};
	return this->_createTexture(xen::Texture::CubeMap,
	                            false, 4, // 4 channel bytes
	                            Vec3u{data.side_length, data.side_length, 6},
	                            slices);
}

const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::CubeArray<float>& data){
	u64 step = data.side_length * data.side_length;
	const void* slices[6] = {
		&data.elements[step * 0], &data.elements[step * 1], &data.elements[step * 2],
		&data.elements[step * 3], &data.elements[step * 4], &data.elements[step * 5],
	};
	return this->_createTexture(xen::Texture::CubeMap,
	                            true, 1, // 1 channel floats
	                            Vec3u{data.side_length, data.side_length, 6},
	                            slices);
}
const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::CubeArray<Vec2f>& data){
	u64 step = data.side_length * data.side_length;
	const void* slices[6] = {
		&data.elements[step * 0], &data.elements[step * 1], &data.elements[step * 2],
		&data.elements[step * 3], &data.elements[step * 4], &data.elements[step * 5],
	};
	return this->_createTexture(xen::Texture::CubeMap,
	                            true, 2, // 1 channel floats
	                            Vec3u{data.side_length, data.side_length, 6},
	                            slices);
}
const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::CubeArray<Vec3f>& data){
	u64 step = data.side_length * data.side_length;
	const void* slices[6] = {
		&data.elements[step * 0], &data.elements[step * 1], &data.elements[step * 2],
		&data.elements[step * 3], &data.elements[step * 4], &data.elements[step * 5],
	};
	return this->_createTexture(xen::Texture::CubeMap,
	                            true, 3, // 3 channel floats
	                            Vec3u{data.side_length, data.side_length, 6},
	                            slices);
}
const xen::Texture* xen::ModuleApiGraphics::createCubeMap(const xen::CubeArray<Vec4f>& data){
	u64 step = data.side_length * data.side_length;
	const void* slices[6] = {
		&data.elements[step * 0], &data.elements[step * 1], &data.elements[step * 2],
		&data.elements[step * 3], &data.elements[step * 4], &data.elements[step * 5],
	};
	return this->_createTexture(xen::Texture::CubeMap,
	                            true, 4, // 4 channel floats
	                            Vec3u{data.side_length, data.side_length, 6},
	                            slices);
}

void xen::ModuleApiGraphics::clear(xen::RenderTarget target, xen::Color color){
	this->pushOp(xen::RenderOp::Clear(target, color));
}

void xen::ModuleApiGraphics::render(xen::RenderTarget target,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::Array<RenderCommand3d> commands
                                   ){
	this->pushOp(xen::RenderOp::Draw(target, viewport, params, commands));
}

void xen::ModuleApiGraphics::render(xen::RenderTarget target,
                                    xen::Aabb2u viewport,
                                    xen::RenderParameters3d& params,
                                    xen::RenderCommand3d& cmd){
	xen::Array<xen::RenderCommand3d> cmd_list = { 1, &cmd };
	this->pushOp(xen::RenderOp::Draw(target, viewport, params, cmd_list));
}

void xen::ModuleApiGraphics::swapBuffers(xen::RenderTarget target){
	this->pushOp(xen::RenderOp::SwapBuffers(target));
}

#endif
