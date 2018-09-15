////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of the GraphicsDevice interface.
/// The methods of this class are the ONLY methods which depend on the
/// underlying render backend - as such to support a new rendering backend
/// just create a new GraphicsDevice derived class
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSDEVICE_HPP
#define XEN_GRAPHICS_GRAPHICSDEVICE_HPP

#include <xen/graphics/GraphicsDevice_types.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/Mesh_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/array.hpp>

namespace xen {
	struct Window;
	struct RawImage;

	/////////////////////////////////////////////////////////////////////
	/// \brief Defines the interface for interacting with a GraphicsDevice
	/// of some kind
	/////////////////////////////////////////////////////////////////////
	class GraphicsDevice {
	protected:
		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a new GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		GraphicsDevice();
	public:
		virtual ~GraphicsDevice();

		// :TODO: create render target
		// :TDOO: create window

		virtual Window* createWindow(Vec2u size, const char* title = "XenoEngine") = 0;
		virtual void    destroyWindow(Window* window) = 0;
		virtual void    swapBuffers(Window* window) = 0;

		/// \defgroup Mesh
		/// @{
		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		/// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		/////////////////////////////////////////////////////////////////////
		virtual Mesh createMesh(const MeshData* mesh_data) = 0;

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
		Mesh createMesh(const VertexSpec&         vertex_spec,
		                const MeshAttribArrays& mesh_geom
		                );

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
		Mesh createMesh(const VertexSpec&         vertex_spec,
		                u32                       vertex_count,
		                ...);

		/////////////////////////////////////////////////////////////////////
		/// \brief Updates the vertex data for a particular attribute of some
		/// mesh
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
		virtual void updateMeshVertexData(Mesh mesh,
		                                  u32 attrib_index,
		                                  void* new_data,
		                                  u32 start_vertex = 0,
		                                  u32 end_vertex   = 0xFFFFFFFF
		                                 ) = 0;

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys a Mesh previously created by this GraphicsDevice
		/// freeing all associated hardware resources
		/////////////////////////////////////////////////////////////////////
		virtual void destroyMesh(Mesh mesh) = 0;
		/// @}


		/// \defgroup Texture
		/// @{

		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		/// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		virtual Texture createTexture(const RawImage* image) = 0;

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys an existing Texture freeing associated resources
		/////////////////////////////////////////////////////////////////////
		virtual void destroyTexture(Texture texture) = 0;
		/// @}


		/// \defgroup Shader
		/// @{

		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a shader program that may be used by the graphics device
		///
		/// \param source Pointer to graphics device implementation specific data
		/// used to create the shader
		///
		/// \todo :TODO: Can we have typesafe creation of a shader without forcing
		/// us to expose the actual graphics device type in use?
		/// Problem is that the data used to create a shader changes (GLSL
		/// source files for OpenGL, function pointer for software renderer, etc)
		/// Can we use an intermediate format and compile down?
		/// SPIRV maybe? -> experimental SPIRV to c++ at:
		/// https://github.com/KhronosGroup/SPIRV-Cross
		///
		/// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		virtual Shader createShader(const void* source) = 0;

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys an existing Shader
		/////////////////////////////////////////////////////////////////////
		virtual void destroyShader(Shader shader) = 0;
		/// @}

		/// \defgroup Drawing Operations
		/// @{
		/////////////////////////////////////////////////////////////////////
		/// \brief Clears a render target to some color
		/// \todo :TODO: parameters such as do we clear the depth buffer?
		/////////////////////////////////////////////////////////////////////
		virtual void clear(RenderTarget& target, xen::Color color) = 0;
		virtual void clear(Window* window, xen::Color color);

		/////////////////////////////////////////////////////////////////////
		/// \brief Renders a series of render commands to some viewport of
		/// some RenderTarget
		/////////////////////////////////////////////////////////////////////
		virtual void render(RenderTarget target,
		                    const xen::Aabb2u& viewport,
		                    const RenderParameters3d& params,
		                    const xen::Array<RenderCommand3d> commands
		                   ) = 0;
		void render(Window* window,
		            const xen::Aabb2u& viewport,
		            const RenderParameters3d& params,
		            const xen::Array<RenderCommand3d> commands
		            );
		/// @}
	};
}

#endif
