////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of rendering to some target using opengl
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDER_CPP
#define XEN_GL_RENDER_CPP

#include "Material.hxx"
#include "Mesh.hxx"
#include "gl_header.hxx"
#include "RenderTarget.hxx"
#include "ModuleGl.hxx"

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/core/array.hpp>
#include <xen/math/utilities.hpp>

void renderMesh(const xgl::MeshGlData* mesh){
	if(mesh == nullptr){
		XenLogWarn("Request to render null mesh, skipping");
		return;
	}

	for(u64 i = 0; i < xen::size(mesh->vertex_spec); ++i){
		if(mesh->vertex_data[i].buffer){
			XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, mesh->vertex_data[i].buffer));
			XEN_CHECK_GL(glEnableVertexAttribArray(i));

			GLint component_count = (
				mesh->vertex_spec[i].type & xen::VertexAttribute::Type::ComponentCountMask
			);

			GLenum component_type;
			GLboolean normalized = GL_FALSE;
			switch(mesh->vertex_spec[i].type & xen::VertexAttribute::Type::ComponentTypeMask){
			case xen::VertexAttribute::Type::Float:
				component_type = GL_FLOAT;
				break;
			case xen::VertexAttribute::Type::Double:
				component_type = GL_DOUBLE;
				break;
			case xen::VertexAttribute::Type::Byte:
				component_type = GL_UNSIGNED_BYTE;
				normalized     = GL_TRUE; // map [0-255] to [0-1]
				break;
			default:
				XenInvalidCodePath("Unhandled vertex attribute type");
				break;
			}

			XEN_CHECK_GL(glVertexAttribPointer(i,
			                                   component_count,
			                                   component_type,
			                                   normalized,
			                                   mesh->vertex_data[i].stride,
			                                   (void*)mesh->vertex_data[i].offset
			                                   )
			             );
		} else {
			XEN_CHECK_GL(glDisableVertexAttribArray(i));

			switch(mesh->vertex_spec[i].type){
			case (xen::VertexAttribute::Type::Float | 3):
				XEN_CHECK_GL(glVertexAttrib3fv(i, &mesh->vertex_data[i].vec3f[0]));
				break;
			case (xen::VertexAttribute::Type::Double | 3):
				XEN_CHECK_GL(glVertexAttrib3dv(i, &mesh->vertex_data[i].vec3d[0]));
				break;
			case (xen::VertexAttribute::Type::Float | 2):
				XEN_CHECK_GL(glVertexAttrib2fv(i, &mesh->vertex_data[i].vec2f[0]));
				break;
			case (xen::VertexAttribute::Type::Double | 2):
				XEN_CHECK_GL(glVertexAttrib2dv(i, &mesh->vertex_data[i].vec2d[0]));
				break;
			case (xen::VertexAttribute::Type::Byte | 4): {
				// :TODO: if someone uses custom 4 byte attribute and doesn't want it to
				// be mapped in this way, then what?
				XenDebugAssert(mesh->vertex_spec[i].aspect == xen::VertexAttribute::Color,
				               "Expected 4 component byte vector only supports color aspect");
				xen::Color color = mesh->vertex_data[i].color4b;
				XEN_CHECK_GL(glVertexAttrib4f
				             (i,
				              xen::mapToRange<u32, float>(0, 255, 0.0f, 1.0f, color.r),
				              xen::mapToRange<u32, float>(0, 255, 0.0f, 1.0f, color.g),
				              xen::mapToRange<u32, float>(0, 255, 0.0f, 1.0f, color.b),
				              xen::mapToRange<u32, float>(0, 255, 0.0f, 1.0f, color.a)
				              ));
				break;
			}
			default:
				XenInvalidCodePath("Unhandled vertex attribute type");
			}
		}
	}

	GLenum prim_type;
	switch(mesh->primitive_type & xen::PrimitiveType::TypeMask){
	case xen::PrimitiveType::Points    : prim_type = GL_POINTS;     break;
	case xen::PrimitiveType::Lines     : prim_type = GL_LINES;      break;
	case xen::PrimitiveType::LineStrip : prim_type = GL_LINE_STRIP; break;
	case xen::PrimitiveType::Triangles : prim_type = GL_TRIANGLES;  break;
	case xen::PrimitiveType::Patch:
		prim_type = GL_PATCHES;
		glPatchParameteri(GL_PATCH_VERTICES,
		                  mesh->primitive_type & xen::PrimitiveType::PatchCountMask);
		break;
	default:
		XenBreak("Unknown primitive type requested");
		break;
	}

	XEN_CHECK_GL(glDrawArrays(prim_type, 0, mesh->vertex_count));
}

namespace xgl {
	void render(xen::RenderTarget render_target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) {

		xgl::RenderTargetImpl* target = getRenderTargetImpl(render_target);
		xgl::makeCurrent(target);

		Vec2u viewport_size = viewport.max - viewport.min;
		glViewport(viewport.min.x, viewport.min.y, viewport_size.x, viewport_size.y);

		xen::impl::checkGl(__LINE__, __FILE__);
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d* cmd = &commands[cmd_index];

			if(cmd->material == nullptr){
				xgl::applyMaterial(xgl::state->default_material, *cmd, params, viewport);
			} else {
				xgl::applyMaterial((xgl::Material*)cmd->material, *cmd, params, viewport);
			}

			switch(cmd->draw_mode){
			case xen::RenderCommand3d::Filled:
				XGL_CHECK(glPolygonMode(GL_FRONT_AND_BACK, GL_FILL));
				break;
			case xen::RenderCommand3d::Wireframe:
				XGL_CHECK(glPolygonMode(GL_FRONT_AND_BACK, GL_LINE));
				break;
			case xen::RenderCommand3d::PointCloud:
				XGL_CHECK(glPolygonMode(GL_FRONT_AND_BACK, GL_POINT));
				break;
			}

			switch(cmd->cull_mode){
			case xen::RenderCommand3d::CullBack:
				XGL_CHECK(glEnable  (GL_CULL_FACE ));
				XGL_CHECK(glCullFace(GL_BACK      ));
				break;
			case xen::RenderCommand3d::CullFront:
				XGL_CHECK(glEnable  (GL_CULL_FACE ));
				XGL_CHECK(glCullFace(GL_FRONT     ));
				break;
			case xen::RenderCommand3d::CullNone:
				XGL_CHECK(glDisable (GL_CULL_FACE ));
				break;
			}

			renderMesh((const xgl::MeshGlData*)cmd->mesh);
		}
	}
}

#endif
