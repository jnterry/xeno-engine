////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of rendering to some target using opengl
///
/// \ingroup module-gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_RENDER_CPP
#define XEN_GL_RENDER_CPP

#include "Shader.hxx"
#include "Mesh.hxx"
#include "gl_header.hxx"
#include "RenderTarget.hxx"
#include "ModuleGl.hxx"

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/core/array.hpp>
#include <xen/math/utilities.hpp>


GLenum xenPrimitiveTypeToGl(xen::PrimitiveType type){
	switch(type){
	case xen::PrimitiveType::POINTS     : return GL_POINTS;
	case xen::PrimitiveType::LINES      : return GL_LINES;
	case xen::PrimitiveType::LINE_STRIP : return GL_LINE_STRIP;
	case xen::PrimitiveType::TRIANGLES  : return GL_TRIANGLES;
	}
	XenInvalidCodePath("Unhandled xen::PrimtiveType in GlDevice");
	return 0;
}

void renderMesh(xen::PrimitiveType primitive_type,
                const xgl::MeshGlData* mesh){
	for(u64 i = 0; i < xen::size(mesh->vertex_spec); ++i){
		if(mesh->vertex_data[i].buffer){
			XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, mesh->vertex_data[i].buffer));
			XEN_CHECK_GL(glEnableVertexAttribArray(i));

			GLint component_count = (mesh->vertex_spec[i] &
			                         xen::VertexAttribute::_ComponentCountMask
			                         );

			GLenum component_type;
			GLboolean normalized = GL_FALSE;
			switch(mesh->vertex_spec[i] & xen::VertexAttribute::_TypeMask){
			case xen::VertexAttribute::_TypeFloat:
				component_type = GL_FLOAT;
				break;
			case xen::VertexAttribute::_TypeDouble:
				component_type = GL_DOUBLE;
				break;
			case xen::VertexAttribute::_TypeByte:
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

			switch(mesh->vertex_spec[i]){
			case xen::VertexAttribute::Position3r:
			case xen::VertexAttribute::Normal3r:
#if XEN_USE_DOUBLE_PRECISION
				XEN_CHECK_GL(glVertexAttrib3dv(i, &mesh->vertex_data[i].vec3r[0]));
#else
				XEN_CHECK_GL(glVertexAttrib3fv(i, &mesh->vertex_data[i].vec3r[0]));
#endif
				break;
			case xen::VertexAttribute::TexCoord2f:
				XEN_CHECK_GL(glVertexAttrib2fv(i, &mesh->vertex_data[i].vec2f[0]));
				break;
			case xen::VertexAttribute::Color3f:
				XEN_CHECK_GL(glVertexAttrib3fv(i, &mesh->vertex_data[i].color3f[0]));
				break;
			case xen::VertexAttribute::Color4b: {
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

	XEN_CHECK_GL(glDrawArrays(xenPrimitiveTypeToGl(primitive_type),
	                          0,
	                          mesh->vertex_count));
}


namespace xgl {
	void render(xen::RenderTarget render_target,
	            const xen::Aabb2u& viewport,
	            const xen::RenderParameters3d& params,
	            const xen::Array<xen::RenderCommand3d> commands
	            ) {

		xen::gl::RenderTargetImpl* target = getRenderTargetImpl(render_target);
		xen::gl::makeCurrent(target);

		Vec2u viewport_size = viewport.max - viewport.min;
		glViewport(viewport.min.x, viewport.min.y, viewport_size.x, viewport_size.y);

		xgl::ShaderProgram* prog = &xgl::gl_state->pool_shader.slots[0].item;
		xgl::useShader(prog);

		int mvp_mat_loc           = xgl::getUniformLocation(prog, "mvp_mat"          );
		int model_mat_loc         = xgl::getUniformLocation(prog, "model_mat"        );
		int point_light_pos_loc   = xgl::getUniformLocation(prog, "point_light_pos"  );
		int point_light_color_loc = xgl::getUniformLocation(prog, "point_light_color");
		int emissive_color_loc    = xgl::getUniformLocation(prog, "emissive_color"   );
		int camera_pos_loc        = xgl::getUniformLocation(prog, "camera_position"  );

		if(xen::size(params.lights) && params.lights.elements != nullptr){
			// :TODO: support multiple lights
			xgl::setUniform(point_light_pos_loc,   params.lights[0].point.position);
			xgl::setUniform(point_light_color_loc, params.lights[0].color);

			xgl::setUniform(emissive_color_loc,    Vec4f::Origin);
		} else {
			xgl::setUniform(point_light_pos_loc,   Vec3r::Origin);
			xgl::setUniform(point_light_color_loc, Vec4f::Origin);
			xgl::setUniform(emissive_color_loc,    xen::Color::WHITE4f);
		}

		xgl::setUniform(camera_pos_loc, xen::getCameraPosition(params.camera));

		Mat4r vp_mat = (getViewMatrix(params.camera) *

		                // opengl has (0,0) at bottom left, we expect it to be at top left so flip
		                xen::Scale3d(1, -1, 1)       *
		                getProjectionMatrix(params.camera, (Vec2r)(viewport.max - viewport.min))
		                );

		xen::impl::checkGl(__LINE__, __FILE__);
		for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
			const xen::RenderCommand3d* cmd = &commands[cmd_index];

			XEN_CHECK_GL(glBindTexture(GL_TEXTURE_2D, xgl::getTextureImpl(cmd->textures[0])->id));

			xgl::setUniform(mvp_mat_loc,        cmd->model_matrix * vp_mat);
			xgl::setUniform(model_mat_loc,      cmd->model_matrix);
			xgl::setUniform(emissive_color_loc, cmd->emissive_color);
			renderMesh(cmd->primitive_type, xgl::getMeshGlData(cmd->mesh));
		}
	}
}

#endif
