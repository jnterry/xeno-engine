////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines the OpenGL Graphics Device
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GL_GLDEVICE_CPP
#define XEN_GL_GLDEVICE_CPP

#include <xen/graphics/GraphicsDevice.hpp>
#include <xen/graphics/Mesh.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <xen/core/array.hpp>
#include <xen/core/File.hpp>
#include <xen/math/utilities.hpp>
#include <xen/config.hpp>

#include "gl_header.hxx"
#include "Mesh.hxx"
#include "Shader.hxx"
#include "Texture.hxx"
#include "../graphics/Window.hxx"

#include <GL/glew.h>

#if defined XEN_OS_UNIX
	#include "RenderTarget.unix.cpp"
#elif defined XEN_OS_WINDOWS
	#include "RenderTarget.win.cpp"
#else
	// :TODO: add dummy implementation
	#error "GlDevice is not implemented on this platform!"
#endif

namespace {

		/*void xenGlDebugCallback(GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length, const GLchar* msg, onst void* userParam){
		  const char* source_str = "Unknown Source";
		  const char* type_str   = "Unknown Type";

		  switch(source){
		  case DEBUG_SOURCE_API:
		  source_str = "DEBUG_SOURCE_API (call to gl function)"; break;
		  case DEBUG_SOURCE_WINDOW_SYSTEM:
		  source_str = "DEBUG_SOURCE_WINDOW_SYSTEM"; break;
		  case DEBUG_SOURCE_SHADER_COMPILER:
		  source_str = "DEBUG_SOURCE_SHADER_COMPILER"; break;
		  case DEBUG_SOURCE_THIRD_PARTY:
		  source_str = "DEBUG_SOURCE_THRID_PARTY"; break;
		  case DEBUG_SOURCE_APPLICATION:
		  source_str = "DEBUG_SOURCE_APPLICATION"; break;
		  case DEBUG_SOURCE_OTHER:
		  source_str = "DEBUG_SOURCE_OTHER";break;
		  default: XenInvalidPath;
		  };

		  switch(type){
		  case DEBUG_TYPE_ERROR:               type_str = "DEBUG_TYPE_ERROR"; break;
		  case DEBUG_TYPE_DEPRECATED_BEHAVIOR: type_str = "DEBUG_TYPE_DEPRECATED_BEHAVIOR"; break;
		  case DEBUG_TYPE_UNDEFINED_BEHAVIOR:  type_str = "DEBUG_TYPE_UNDEFINED_BEHAVIOR"; break;
		  case DEBUG_TYPE_PORTABILITY:         type_str = "DEBUG_TYPE_PORTABILITY"; break;
		  case DEBUG_TYPE_PERFORMANCE:         type_str = "DEBUG_TYPE_PERFORMANCE"; break;
		  case DEBUG_TYPE_MARKER:              type_str = "DEBUG_TYPE_MARKER"; break;
		  case DEBUG_TYPE_PUSH_GROUP:          type_str = "DEBUG_TYPE_PUSH_GROUP"; break;
		  case DEBUG_TYPE_POP_GROUP:           type_str = "DEBUG_TYPE_POP_GROUP"; break;
		  case DEBUG_TYPE_OTHER:               type_str = "DEBUG_TYPE_OTHER"; break;
		  default: XenInvalidPath;
		  };

		  xen::log::write(::xen::_sys::log, "?", 0, xen::log::ERROR, "OpenGL Error Callback: source: '%s', type: '%s', msg: %s",
		  source_str, type_str, msg, "xen.gl", nullptr);

		  }*/

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

	xen::gl::ShaderProgram* loadShader(xen::ArenaLinear& arena){
		XenTempArena(scratch, 8196);

		// :TODO: we can't rely on these glsl files just existing in bin dir...

		xen::FileData vertex_src = xen::loadFileAndNullTerminate(scratch, "vertex.glsl");
		xen::FileData pixel_src  = xen::loadFileAndNullTerminate(scratch, "pixel.glsl");

		auto result = xen::gl::createShaderProgram(arena,
		                                           (char*)&vertex_src[0],
		                                           (char*)&pixel_src[0]);

		if(!xen::gl::isOkay(result)){
			xen::resetArena(scratch);
			const char* errors = xen::gl::getErrors(result, scratch);
			printf("Shader Errors:\n%s\n", errors);
			XenBreak();
		} else {
			printf("Shader compiled successfully\n");
		}

		return result;
	}


	void renderMesh(xen::PrimitiveType primitive_type,
	                const xen::gl::MeshGlData* mesh){
		for(int i = 0; i < mesh->attrib_count; ++i){
			if(mesh->attrib_sources[i].buffer){
				XEN_CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, mesh->attrib_sources[i].buffer));
				XEN_CHECK_GL(glEnableVertexAttribArray(i));

				GLint component_count = (mesh->attrib_types[i] &
				                         xen::VertexAttribute::_ComponentCountMask
				                        );

				GLenum component_type;
				GLboolean normalized = GL_FALSE;
				switch(mesh->attrib_types[i] & xen::VertexAttribute::_TypeMask){
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

				XEN_CHECK_GL(glVertexAttribPointer(i, //attrib layout
				                                   component_count,
				                                   component_type,
				                                   normalized,
				                                   mesh->attrib_sources[i].stride,
				                                   (void*)mesh->attrib_sources[i].offset
				                                   )
				             );
			} else {
				XEN_CHECK_GL(glDisableVertexAttribArray(i));

				switch(mesh->attrib_types[i]){
				case xen::VertexAttribute::Position3r:
				case xen::VertexAttribute::Normal3r:
					#if XEN_USE_DOUBLE_PRECISION
					XEN_CHECK_GL(glVertexAttrib3dv(i, &mesh->attrib_sources[i].vec3r[0]));
					#else
					XEN_CHECK_GL(glVertexAttrib3fv(i, &mesh->attrib_sources[i].vec3r[0]));
					#endif
					break;
				case xen::VertexAttribute::TexCoord2f:
					XEN_CHECK_GL(glVertexAttrib2fv(i, &mesh->attrib_sources[i].vec2f[0]));
					break;
				case xen::VertexAttribute::Color3f:
					XEN_CHECK_GL(glVertexAttrib3fv(i, &mesh->attrib_sources[i].color3f[0]));
					break;
				case xen::VertexAttribute::Color4b: {
					xen::Color color = mesh->attrib_sources[i].color4b;
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

	class GlDevice : public xen::GraphicsDevice {
	private:
		xen::Allocator* main_allocator;

		xen::FixedArray<xen::gl::MeshGlData*, 256> mesh_store;
		xen::ArenaLinear mesh_header_arena;

		xen::FixedArray<xen::gl::RenderTargetImpl*, 128> render_targets;

		xen::ArenaPool<xen::gl::TextureImpl> textures;

		xen::ArenaLinear misc_arena;

		xen::gl::ShaderProgram* prog;

		xen::gl::RenderTargetImpl* getRenderTargetImpl(xen::RenderTarget target){
			xen::gl::RenderTargetImpl* result = render_targets[target._id];
			XenAssert(result != nullptr, "Target must be valid");
			return result;
		}
	public:
		~GlDevice(){
			// :TODO: free all GPU resources (eg, mesh data buffers)
			xen::destroyArenaLinear(*main_allocator, mesh_header_arena);
			xen::destroyArenaLinear(*main_allocator, misc_arena);
			delete main_allocator;
		}

		GlDevice()
			: main_allocator(new xen::AllocatorCounter<xen::AllocatorMalloc>()),
			  mesh_header_arena(xen::createArenaLinear(*main_allocator, xen::megabytes(1))),
			  misc_arena       (xen::createArenaLinear(*main_allocator, xen::megabytes(1))),
			  textures         (xen::createArenaPool<xen::gl::TextureImpl>(main_allocator, 1024))
		{

			// :TODO: better way of managing mesh headers:
			// - remove fixed limit MESH_STORE_SIZE
			// - deal with mesh generations (IE: re-using a mesh id)
			// - mesh_header_arena will currently just slowly fill up, need way to
			//   reclaim space when a mesh is deleted
			for(u32 i = 0; i < xen::size(mesh_store); ++i){
				mesh_store[i] = nullptr;
			}
			for(u32 i = 0; i < xen::size(render_targets); ++i){
				render_targets[i] = nullptr;
			}
		}

		xen::Window* createWindow(Vec2u size, const char* title){
			xen::MemoryTransaction transaction(misc_arena);
			printf("About to create window\n");
			xen::Window* window = xen::impl::createWindow(misc_arena, size, title);

			xen::gl::RenderTargetImpl* render_target = xen::gl::createWindowRenderTarget(misc_arena, window);

			u32 slot;
			for(slot = 0; slot < xen::size(mesh_store); ++slot){
				if(render_targets[slot] == nullptr){
					break;
				}
			}
			XenAssert(slot < xen::size(mesh_store), "Render target store full");

			render_targets[slot] = render_target;
			xen::RenderTarget result = makeHandle<xen::RenderTarget::HANDLE_ID>(slot, 0);
			window->render_target = result;

			printf("Making render target current\n");
			xen::gl::makeCurrent(render_target);

			// Initialize glew to get GL extensions
			if(glewInit() != GLEW_OK){
				// :TODO: log
				printf("ERROR: GLEW Init failed\n");
				return nullptr;
			}

			// :TODO: log
			int major = 0;
			int minor = 0;
			glGetIntegerv(GL_MAJOR_VERSION, &major);
			glGetIntegerv(GL_MINOR_VERSION, &minor);
			printf("OpenGL context created:\n - Version %d.%d\n - Vendor %s\n - Renderer %s\n",
			       major, minor,
			       glGetString(GL_VENDOR),
			       glGetString(GL_RENDERER));

			printf("Doing GL setup\n");
			///////////////////////////////////////////////////
			// Do GL setup
			// :TODO: something better with shaders -> ideally expose them to user of xenogin
			// but how do "programable pipeline" in software / other devices?
			this->prog = loadShader(mesh_header_arena);

			XEN_CHECK_GL(glEnable   (GL_DEPTH_TEST));
			XEN_CHECK_GL(glDepthFunc(GL_LESS      ));

			XEN_CHECK_GL(glEnable   (GL_CULL_FACE ));
			XEN_CHECK_GL(glFrontFace(GL_CCW       ));
			XEN_CHECK_GL(glCullFace (GL_BACK      ));

			// :TODO: -> needed to use vertex array in core context, but don't know anything
			// more about this than that fact. See:
			// https://stackoverflow.com/a/13405205
			GLuint vao;
			glGenVertexArrays(1, &vao);
			glBindVertexArray(vao);

			//:TODO: compile time if def, 3 levels for gl debugging:
			// none -> no checks
			// some -> do this setup here
			// all  -> enable the XEN_CHECK_GL macros
			//glEnable(GL_DEBUG_OUTPUT);
			//glDebugMessageCallback(&impl::xenGlDebugCallback, nullptr);

			printf("Done GL setup\n");

			transaction.commit();
			return window;
		}
		void destroyWindow(xen::Window* window){
			xen::gl::RenderTargetImpl* target = getRenderTargetImpl(window->render_target);
			xen::gl::destroyRenderTarget(target);
			render_targets[window->render_target._id] = nullptr;

			xen::impl::destroyWindow(window);
			xen::clearToZero<xen::Window>(window);
		}
		void swapBuffers(xen::Window* window){
			if(window->is_open){
				xen::gl::swapBuffers(this->getRenderTargetImpl(window->render_target));
			}
		}

		xen::Mesh createMesh(const xen::MeshData* mesh_data){
			// :TODO:COMP:ISSUE_31: Object pool helper
			u32 slot;
			for(slot = 0; slot < xen::size(mesh_store); ++slot){
				if(mesh_store[slot] == nullptr){
					break;
				}
			}
			XenAssert(slot < xen::size(mesh_store), "Mesh store full, cannot create new mesh");

			mesh_store[slot] = xen::gl::createMesh(mesh_header_arena, *mesh_data);

			return makeHandle<xen::Mesh::HANDLE_ID>(slot, 0);
		}

		void updateMeshAttribData(xen::Mesh mesh_handle,
		                          u32 attrib_index,
		                          void* new_data,
		                          u32 start_vertex,
		                          u32 end_vertex
		                          ) {
			xen::gl::MeshGlData* mesh = mesh_store[mesh_handle._id];
			xen::gl::updateMeshAttribData(mesh, attrib_index, new_data,
			                              start_vertex, end_vertex
			                             );
		}

		void destroyMesh(xen::Mesh mesh) {
			// :TODO: IMPLEMENT - currently resource link, GPU buffers needs destroying
			mesh_store[mesh._id] = nullptr;
		}


		xen::gl::TextureImpl* getTextureImpl(const xen::Texture texture){
			return &this->textures.slots[texture._id].item;
		}

		xen::Texture createTexture(const xen::RawImage* image) override {
			u32 slot = xen::reserveSlot(textures);

			xen::Texture result = this->makeHandle<xen::Texture::HANDLE_ID>(slot, 0);
		  xen::gl::loadTexture(image, getTextureImpl(result));

			return result;
		}

		void destroyTexture(xen::Texture texture) override {
			xen::gl::deleteTexture(getTextureImpl(texture));
		}

		xen::Shader createShader(const void* source) override {
			return xen::makeNullHandle<xen::Shader>();
			// :TODO: implement
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys an existing Shader
		/////////////////////////////////////////////////////////////////////
		void destroyShader(xen::Shader texture) override {
			// :TODO: implement
		}

		void clear(xen::RenderTarget& render_target, xen::Color color) override {
			xen::gl::RenderTargetImpl* target = getRenderTargetImpl(render_target);
			xen::gl::makeCurrent(target);

			xen::Color4f color01 = (xen::Color4f)color;
			XEN_CHECK_GL(glClearColor(color01.r, color01.g, color01.b, 1));
			XEN_CHECK_GL(glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT));
		}

		void render(xen::RenderTarget render_target,
		            const xen::Aabb2u& viewport,
		            const xen::RenderParameters3d& params,
		            const xen::Array<xen::RenderCommand3d> commands
		            ) {

			xen::gl::RenderTargetImpl* target = getRenderTargetImpl(render_target);
			xen::gl::makeCurrent(target);

			Vec2u viewport_size = viewport.max - viewport.min;
			glViewport(viewport.min.x, viewport.min.y, viewport_size.x, viewport_size.y);

			xen::gl::useShader(prog);

			int mvp_mat_loc           = xen::gl::getUniformLocation(prog, "mvp_mat"          );
			int model_mat_loc         = xen::gl::getUniformLocation(prog, "model_mat"        );
			int point_light_pos_loc   = xen::gl::getUniformLocation(prog, "point_light_pos"  );
			int point_light_color_loc = xen::gl::getUniformLocation(prog, "point_light_color");
			int emissive_color_loc    = xen::gl::getUniformLocation(prog, "emissive_color"   );
			int camera_pos_loc        = xen::gl::getUniformLocation(prog, "camera_position"  );

			if(xen::size(params.lights) && params.lights.elements != nullptr){
				// :TODO: support multiple lights
				xen::gl::setUniform(point_light_pos_loc,   params.lights[0].point.position);
				xen::gl::setUniform(point_light_color_loc, params.lights[0].color);

				xen::gl::setUniform(emissive_color_loc,    Vec4f::Origin);
			} else {
				xen::gl::setUniform(point_light_pos_loc,   Vec3r::Origin);
				xen::gl::setUniform(point_light_color_loc, Vec4f::Origin);
				xen::gl::setUniform(emissive_color_loc,    xen::Color::WHITE4f);
			}

			xen::gl::setUniform(camera_pos_loc, xen::getCameraPosition(params.camera));

			Mat4r vp_mat = (getViewMatrix(params.camera) *

			                // opengl has (0,0) at bottom left, we expect it to be at top left so flip
			                xen::Scale3d(1, -1, 1)       *
			                getProjectionMatrix(params.camera, (Vec2r)(viewport.max - viewport.min))
			               );

			xen::impl::checkGl(__LINE__, __FILE__);
			for(u32 cmd_index = 0; cmd_index < commands.size; ++cmd_index){
				const xen::RenderCommand3d* cmd = &commands[cmd_index];

				xen::gl::setUniform(mvp_mat_loc,        cmd->model_matrix * vp_mat);
				xen::gl::setUniform(model_mat_loc,      cmd->model_matrix);
				xen::gl::setUniform(emissive_color_loc, cmd->emissive_color);
				renderMesh(cmd->primitive_type, mesh_store[cmd->mesh._id]);
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
