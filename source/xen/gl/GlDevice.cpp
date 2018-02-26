////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the OpenGL Graphics Device
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_GLDEVICE_CPP
#define XEN_GL_GLDEVICE_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/util/File.hpp>

#include "gl_header.hxx"
#include "Mesh.hxx"
#include "Shader.hxx"

namespace {
	xen::gl::ShaderProgram* loadShader(xen::ArenaLinear& arena){
		XenTempArena(scratch, 8196);

		// :TODO: we can't rely on these glsl files just existing in bin dir...

		xen::FileData vertex_src = xen::loadFileAndNullTerminate(scratch, "vertex.glsl");
		xen::FileData pixel_src  = xen::loadFileAndNullTerminate(scratch, "pixel.glsl");

		auto result = xen::gl::createShaderProgram(arena, (char*)vertex_src.data, (char*)pixel_src.data);

		if(!xen::gl::isOkay(result)){
			xen::resetArena(scratch);
			const char* errors = xen::gl::getErrors(result, scratch);
			printf("Shader Errors:\n%s\n", errors);
			exit(1);
		} else {
			printf("Shader compiled successfully\n");
		}

		return result;
	}


	void renderMesh(const xen::gl::MeshHeader* mesh){
		for(int i = 0; i < mesh->attribute_count; ++i){
			if(mesh->attribute_sources[i].buffer){
				XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, mesh->attribute_sources[i].buffer));
				XEN_CHECK_GL(glEnableVertexAttribArray(i));
				XEN_CHECK_GL(glVertexAttribPointer(i,           //attrib layout
				                                   3, GL_FLOAT, // num components and type
				                                   GL_FALSE,    // normalised
				                                   mesh->attribute_sources[i].stride,
				                                   (void*)mesh->attribute_sources[i].offset
				                                   )
				             );
			} else {
				XEN_CHECK_GL(glDisableVertexAttribArray(i));
				// :TODO: this relies on real being a float
				XEN_CHECK_GL(glVertexAttrib3f(i,
				                              mesh->attribute_sources[i].vec3r.x,
				                              mesh->attribute_sources[i].vec3r.y,
				                              mesh->attribute_sources[i].vec3r.z
				                              )
				             );
			}
		}

		XEN_CHECK_GL(glDrawArrays(GL_TRIANGLES, 0, mesh->num_triangles * 3));
	}

	static const constexpr u32 MESH_STORE_SIZE = 128;

	class GlDevice : public xen::GraphicsDevice {
	private:
		xen::Allocator* main_allocator;

		xen::FixedArray<xen::gl::MeshHeader*, MESH_STORE_SIZE> mesh_store;
		xen::ArenaLinear mesh_header_arena;

		xen::gl::ShaderProgram* prog;
	public:
		~GlDevice(){
			// :TODO: free all GPU resources (eg, mesh data buffers)
			xen::destroyArenaLinear(*main_allocator, mesh_header_arena);
			delete main_allocator;
		}

		GlDevice()
			: GraphicsDevice(xen::GraphicsDeviceType::OpenGL),
			  main_allocator(new xen::AllocatorCounter<xen::AllocatorMalloc>()),
			  mesh_header_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(1)))
		{

			// :TODO: better way of managing mesh headers:
			// - remove fixed limit MESH_STORE_SIZE
			// - deal with mesh generations (IE: re-using a mesh id)
			// - mesh_header_arena will currently just slowly fill up, need way to
			//   reclaim space when a mesh is deleted
			for(u32 i = 0; i < MESH_STORE_SIZE; ++i){
				mesh_store[i] = nullptr;
			}

			// :TODO: something better with shaders
			this->prog = loadShader(mesh_header_arena);

			///////////////////////////////////////////////////
			// Do GL setup
			XEN_CHECK_GL(glEnable(GL_DEPTH_TEST));
			XEN_CHECK_GL(glDepthFunc(GL_LESS));
		}

		xen::Mesh createMesh(const xen::MeshData& mesh_data){
			u32 slot;
			for(slot = 0; slot < MESH_STORE_SIZE; ++slot){
				if(mesh_store[slot] == nullptr){
					break;
				}
			}

			XenAssert(slot < MESH_STORE_SIZE, "Mesh store full, cannot create new mesh");

			mesh_store[slot] = xen::gl::createMesh(mesh_header_arena,
			                                       mesh_data.attrib_count,
			                                       mesh_data.attrib_types,
			                                       (const void**)mesh_data.attrib_data,
			                                       mesh_data.vertex_count
			                                      );

			return makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);

		}

		void destroyMesh(xen::Mesh mesh) {
			// :TODO: IMPLEMENT - currently resource link, GPU buffers needs destroying
			mesh_store[mesh._id] = nullptr;
		}

		void clear(xen::RenderTarget target,
		           const xen::Aabb2u& viewport,
		           xen::Color color
		           ) override {
			// :TODO: obey the viewport
			xen::Color4f color01 = (xen::Color4f)color;
			XEN_CHECK_GL(glClearColor(color01.r, color01.g, color01.b, 1));
			XEN_CHECK_GL(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));
		}

		void render(xen::RenderTarget target,
		            const xen::Aabb2u& viewport,
		            const xen::RenderParameters3d& params,
		            const xen::Array<xen::RenderCommand3d> commands
		            ) {



			xen::gl::useShader(prog);

			int mvp_mat_loc           = xen::gl::getUniformLocation(prog, "mvp_mat"          );
			int model_mat_loc         = xen::gl::getUniformLocation(prog, "model_mat"        );
			int point_light_pos_loc   = xen::gl::getUniformLocation(prog, "point_light_pos"  );
			int point_light_color_loc = xen::gl::getUniformLocation(prog, "point_light_color");
			int emissive_color_loc    = xen::gl::getUniformLocation(prog, "emissive_color"   );
			int camera_pos_loc        = xen::gl::getUniformLocation(prog, "camera_position"  );

			xen::gl::setUniform(point_light_pos_loc,   params.lights[0].point.position);
			xen::gl::setUniform(point_light_color_loc, params.lights[0].color);

			xen::gl::setUniform(camera_pos_loc, xen::getCameraPosition(params.camera));

			xen::gl::setUniform(emissive_color_loc, Vec4r::Origin);

			Mat4r view_mat = getViewMatrix(params.camera);
			Mat4r proj_mat = getProjectionMatrix(params.camera,
			                                     (Vec2r)(viewport.max - viewport.min));
			Mat4r vp_mat   = view_mat * proj_mat;

			for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
				const xen::RenderCommand3d* cmd = &commands[cmd_index];

				if(cmd->type != xen::RenderCommand3d::MESH){ continue; }

				xen::gl::setUniform(mvp_mat_loc,        cmd->model_matrix * vp_mat);
				xen::gl::setUniform(model_mat_loc,      cmd->model_matrix);
				xen::gl::setUniform(emissive_color_loc, cmd->emissive_color);
				renderMesh((const xen::gl::MeshHeader*)cmd->mesh_header);
			}
		}
		/// @}
	};
}

namespace xen {
	GraphicsDevice* createGlDevice(ArenaLinear& arena){
		return xen::emplace<GlDevice>(arena);
	}
}

#endif
