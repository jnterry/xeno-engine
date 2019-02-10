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

	/// \brief Bundle of arguments that describe to the rendering backend how to
	/// create a material
	struct MaterialCreationParameters {
		/// \brief Array of shader source file paths to be compiled to form
		/// the vertex stage of the shader program associated with this material
		xen::Array<const char*> vertex_sources;

		/// \brief Tessellation control sources
		xen::Array<const char*> tess_ctrl_sources;

		/// \brief Tessellation evaluation sources
		xen::Array<const char*> tess_eval_sources;

		xen::Array<const char*> geometry_sources;


		/// \brief Array of files to be compiled as the pixel shader
		xen::Array<const char*> pixel_sources;

		/// \brief List of sources from which shader parameter values
		/// will be derived
		xen::Array<xen::MaterialParameterSource> parameter_sources;
	};

	/// \brief The vertex spec for the engine's internal default material
	extern VertexSpec DefaultVertexSpec;

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

		const Mesh* (*_createMeshFromMeshData)(const MeshData* mesh_data);
		void        (*destroyMesh           )(const Mesh* mesh);


		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a dynamic mesh which is stored in such a way that
		/// subsequent calls to updateMeshVertexData are fast(er than otherwise),
		/// and additionally such that the number of vertices in the Mesh can vary
		/// (up to some specified maximum). Note that updateMeshVertexData MUST
		/// be called before rendering the mesh for each attribute - otherwise
		/// whatever random data is currently in the graphic's devices memory will
		/// be used!
		/////////////////////////////////////////////////////////////////////
		const Mesh*(*createDynamicMesh)(const VertexSpec& vertex_spec,
		                                const u16 primitive_type,
		                                u32 max_vertex_count);

		/////////////////////////////////////////////////////////////////////
		/// \brief Updates the vertex count of a dynamic mesh. Returns false
		/// if vertex_count exceeds the max_vertex_count which the mesh was
		/// created with, or if the mesh was not created as a dynamic mesh
		/// \note Existing vertex data will not be modified - if the vertex count
		/// is shrunk then the first "vertex_count" elements will be kept, if
		/// instead the vertex count is increased then all existing data will
		/// be kept and new uninitialised vertices will be appended on the end
		/////////////////////////////////////////////////////////////////////
		bool (*setDynamicMeshVertexCount)(const Mesh* mesh, u32 vertex_count);

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
		void    (*_updateMeshVertexData  )(const Mesh* mesh,
		                                   u32 attrib_index,
		                                   void* new_data,
		                                   u32 start_vertex,
		                                   u32 end_vertex);

		Texture (*createTexture )(const RawImage* image);
		void    (*destroyTexture)(Texture texture);

		/// \brief Creates a material which may later be used for rendering geometry
		const Material* (*createMaterial )(const MaterialCreationParameters& params);
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
		inline const xen::Mesh* createMesh(const MeshData* mesh_data){
			return this->_createMeshFromMeshData(mesh_data);
		}

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
		const Mesh* createMesh(const VertexSpec& vertex_spec,
		                       const xen::PrimitiveType primitive_type,
		                       u32 vertex_count,
		                       ...);

		inline void updateMeshVertexData(const Mesh* mesh,
		                                 u32 attrib_index,
		                                 void* new_data,
		                                 u32 start_vertex = 0,
		                                 u32 end_vertex = 0xFFFFFFFF
		                                 ){
			this->_updateMeshVertexData(mesh, attrib_index, new_data, start_vertex, end_vertex);
		}

		void clear(xen::RenderTarget target, xen::Color color);
		void render(xen::RenderTarget, xen::Aabb2u viewport,
		            xen::RenderParameters3d& params,
		            xen::Array<RenderCommand3d> commands);
		void render(xen::RenderTarget, xen::Aabb2u viewport,
		            xen::RenderParameters3d& params,
		            xen::RenderCommand3d& cmd);
		void swapBuffers(xen::RenderTarget window);
	};
}

#endif
