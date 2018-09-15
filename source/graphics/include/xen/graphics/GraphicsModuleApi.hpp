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

#include <xen/graphics/GraphicsDevice_types.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Mesh_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/array.hpp>

namespace xen {
	struct Window;
	struct RawImage;

	/////////////////////////////////////////////////////////////////////
	/// \brief Type representing the Api that a graphics module is expected
	/// to expose
	/////////////////////////////////////////////////////////////////////
	struct GraphicsModuleApi {
		Window* (*createWindow )(Vec2u size, const char* title);
		void    (*destroyWindow)(Window* window);
		void    (*swapBuffers  )(Window* window);

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

		/// \brief Creates a shader from some source type that is dependent on the
		/// backend graphics api
		Shader  (*createShader  )(const void* source);
		void    (*destroyShader )(Shader shader     );

		void    (*_clearTarget   )(RenderTarget window, xen::Color color);

		void    (*_renderToTarget)(RenderTarget                 window,
		                          const Aabb2u&                viewport,
		                          const RenderParameters3d&    params,
		                          const Array<RenderCommand3d> commands
		                         );

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
		Mesh createMesh(const VertexSpec& vertex_spec, const MeshGeometrySource& mesh_geom);

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

		inline void clear(RenderTarget& target, xen::Color color){
		  this->_clearTarget(target, color);
		}
		void clear(Window* window,       xen::Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Renders a series of render commands to some viewport of
		/// some RenderTarget
		/////////////////////////////////////////////////////////////////////
		inline void render(RenderTarget target,
		            const xen::Aabb2u& viewport,
		            const RenderParameters3d& params,
		            const xen::Array<RenderCommand3d> commands
		            ){
			this->_renderToTarget(target, viewport, params, commands);
		}
		void render(Window* window,
		            const xen::Aabb2u& viewport,
		            const RenderParameters3d& params,
		            const xen::Array<RenderCommand3d> commands
		           );

		inline void updateMeshVertexData(Mesh mesh,
		                                 u32 attrib_index,
		                                 void* new_data,
		                                 u32 start_vertex = 0,
		                                 u32 end_vertex = 0xFFFFFFFF
		                                 ){
			this->_updateMeshVertexData(mesh, attrib_index, new_data, start_vertex, end_vertex);
		}
	};
}

#endif
