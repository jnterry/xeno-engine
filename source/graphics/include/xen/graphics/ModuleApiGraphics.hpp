////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of the Api that should be exposed by any
/// rendering module
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSMODULEAPI_HPP
#define XEN_GRAPHICS_GRAPHICSMODULEAPI_HPP

#include <xen/graphics/GraphicsHandles.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Mesh_types.hpp>
#include <xen/graphics/Material_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/array.hpp>

namespace xen {
	struct Window;
	struct RawImage;

	/// \brief Structure which represents some rendering operation
	/// that may be performed by the graphics module
	struct RenderOp {
		enum Type {
			CLEAR,
			DRAW,
			SWAP_BUFFERS,
		};

		Type type;

		struct ClearParams {
			xen::RenderTarget target;
			xen::Color color;
		};

		struct DrawParams {
			xen::RenderTarget            target;
			xen::Aabb2u                  viewport;
			xen::RenderParameters3d*     params;
			xen::Array<RenderCommand3d>  commands;
		};

		struct SwapBufferParams {
			xen::RenderTarget target;
		};

		union {
			ClearParams      clear;
			DrawParams       draw;
			SwapBufferParams swap_buffers;
		};

		// Named constructors for RenderOp
		static RenderOp Clear(xen::RenderTarget, xen::Color color);
		static RenderOp Draw(xen::RenderTarget, xen::Aabb2u viewport, xen::RenderParameters3d& params, xen::Array<RenderCommand3d>& commands);
		static RenderOp SwapBuffers(xen::RenderTarget target);
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Bundle of arguments used to create a shader. This includes
	/// parameters for all rendering backends, and combination of which may
	/// be supplied. Rendering backends should attempt to construct a shader
	/// from that which is not nullptr, and if that is not possible (eg, only
	/// sren shader is specified but we are using opengl) then the backend should
	/// fall back to using its default shader
	///
	/// \todo Something better for creating shaders in a backend agnostic way...
	/////////////////////////////////////////////////////////////////////
	struct ShaderSource {
		/// \brief Pointer to a FragmentShader for sren backends
		void* sren;

		/// \brief Path of glsl code for vertex shader
		const char* glsl_vertex_path;

		/// \brief Path of glsl code for fragment shader
		const char* glsl_fragment_path;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Type representing the Api that a graphics module is expected
	/// to expose
	/////////////////////////////////////////////////////////////////////
	struct ModuleApiGraphics {
		static const constexpr char* const NAME = "graphics";

		xen::RenderTarget (*createWindowRenderTarget)(Window* window);
		void              (*destroyRenderTarget)(xen::RenderTarget target);

		// :TODO:
		//RenderTarget* createRenderTarget(Vec2u size, int depth_bits);
		//void resizeRenderTarget(RenderTarget* render_target);

		Mesh    (*_createMeshFromMeshData)(const MeshData* mesh_data);
		void    (*destroyMesh           )(Mesh mesh);

		/////////////////////////////////////////////////////////////////////
		/// \brief Updates the mesh vertex data for a particular attribute
		/// of some mesh
		///
		/// \param mesh The mesh whose data you wish to modify
		/// \param attrib_index The index of the attribute of each vertex you wish to modify
		/// \param new_data     Pointer to the new data for the attribute
		/// \param start_index  The first vertex you wish to modify, defaults to 0
		/// \param end_vertex   The last vertex you wish to modify, defaults to max int
		/// Note that end_vertex is clamped to be less than or equal to the number of
		/// vertices in the mesh.
		///
		/// \note new_data should contain data for at least (end_vertex - start_vertex)
		/// vertices
		/////////////////////////////////////////////////////////////////////
		void    (*_updateMeshVertexData  )(Mesh mesh,
		                                  u32 attrib_index,
		                                  void* new_data,
		                                  u32 start_vertex,
		                                  u32 end_vertex
		                                 );

		Texture (*createTexture )(const RawImage* image);
		void    (*destroyTexture)(Texture texture);

		Shader  (*createShader  )(const ShaderSource& source);
		void    (*destroyShader )(Shader shader     );

		/// \brief Creates a material which may later be used for rendering geometry
		/// \param source -> How to create the shader which will be used for
		/// rendering
		/// \param params -> Sources to use as parameters for the shader. Any
		/// parameter for which a source is not picked will be added to the
		/// dynamically assignable parameter set for the material. If a source
		/// makes reference to a variable name which does not exist in the shader
		/// it will be ignored
		const Material* (*createMaterial)(const ShaderSource& source,
		                           xen::Array<MaterialParameterSource>& params);
		void            (*destroyMaterial)(const Material* material);

		/// \brief Pushes some rendering operation
		/// Depending on the operation and graphics api in use, this may cause
		/// rendering to take place immediately, or to place it in some internal
		/// queue. In either case, it is guarantied the operation will be performed
		/// at least by the point the tick() function of the graphics module
		/// terminates, and the visible behaviour will be as though the operation
		/// were completed immediately as it was called
		void (*pushOp)(const xen::RenderOp& op);

		/////////////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////////////
		/////////////////////////////////////////////////////////////////////

		// Various overloads that wrap the underlying api calls above
		// only necessary since we can't "overload" function pointers

		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		/// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		/////////////////////////////////////////////////////////////////////
		inline Mesh createMesh(const MeshData* mesh_data){
			return this->_createMeshFromMeshData(mesh_data);
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		///
		/// \param vertex_spec The vertex spec for the created mesh. The created
		/// mesh may rely on this memory, do not free while the mesh exists.
		///
		/// \param mesh_geom   The mesh's geometry
		///
	  /// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		///
		/// \todo :TODO: -> remove the limitation of created mesh relying on
		/// vertex_spec. This is in software devices which don't make deep copy
		/// (not sure about gl device?)
		/////////////////////////////////////////////////////////////////////
		Mesh createMesh(const VertexSpec& vertex_spec, const MeshAttribArrays& mesh_geom);

		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		///
	  /// \param vertex_spec The vertex spec for the created mesh. The created
		/// mesh may rely on this memory, do not free while the mesh exists.
		///
		/// \param vertex_count The number of vertices, IE: the lengths of the
		/// vertex_data arrays
		///
		/// \param  varargs list of void* representing pointer to first
		/// element of data buffer for each vertex attribute. This function expects
		/// the length of the varargs list to be equal to the length of vertex_spec
		///
	  /// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		///
		/// \todo :TODO: -> remove the limitation of created mesh relying on
		/// vertex_spec. This is in software devices which don't make deep copy
		/// (not sure about gl device?)
		/////////////////////////////////////////////////////////////////////
		Mesh createMesh(const VertexSpec& vertex_spec, u32 vertex_count, ...);

		inline void updateMeshVertexData(Mesh mesh,
		                                 u32 attrib_index,
		                                 void* new_data,
		                                 u32 start_vertex = 0,
		                                 u32 end_vertex = 0xFFFFFFFF
		                                 ){
			this->_updateMeshVertexData(mesh, attrib_index, new_data, start_vertex, end_vertex);
		}

		void clear(xen::RenderTarget target, xen::Color color);
		void render(xen::RenderTarget, xen::Aabb2u viewport, xen::RenderParameters3d& params, xen::Array<RenderCommand3d> commands);
		void swapBuffers(xen::RenderTarget window);
	};
}

#endif
