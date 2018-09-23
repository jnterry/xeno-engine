////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Definition of software rasterizer rendering functions
///
/// \ingroup module-sren-texture
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_TEXTURE_CPP
#define XEN_MODULESRENRASTERIZE_TEXTURE_CPP

#include "render.hxx"
#include "Mesh.hxx"
#include "Texture.hxx"
#include "Shader.hxx"
#include "RenderTarget.hxx"
#include <xen/sren/rasterizer3d.hxx>
#include <xen/sren/RenderTarget.hxx>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/math/geometry.hpp>

void xsr::render(xen::RenderTarget target_handle,
                 const xen::Aabb2u& viewport,
                 const xen::RenderParameters3d& params,
                 const xen::Array<xen::RenderCommand3d> commands
                ){

	xsr::RenderTarget& target = *xsr::getRenderTargetImpl(target_handle);

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

		context.fragment_shader = xsr::getShaderImpl(cmd.shader);
		if(context.fragment_shader == nullptr){
			context.fragment_shader = xsr::FragmentShader_Default;
		}
		setPerCommandFragmentUniforms(context,
		                              (xen::Material&)cmd,
		                              cmd.model_matrix,
		                              vp_matrix
		                              );
		context.textures[0] = xsr::getTextureImpl(cmd.textures[0]);
		context.textures[1] = xsr::getTextureImpl(cmd.textures[1]);
		context.textures[2] = xsr::getTextureImpl(cmd.textures[2]);
		context.textures[3] = xsr::getTextureImpl(cmd.textures[3]);
		auto mesh = xsr::getMeshImpl(cmd.mesh);

		#if 0
		renderDebugBoundingBox(context,
		                       xen::getTransformed(mesh->bounds, commands[cmd_index].model_matrix)
		                       );
		#endif

		rasterizeMesh(context, cmd.primitive_type, *mesh);
	}
}

#endif
