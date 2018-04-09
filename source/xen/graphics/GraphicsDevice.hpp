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
#include <xen/math/geometry_types.hpp>
#include <xen/core/array.hpp>

namespace xen {
	struct MeshData;
	struct Window;

	// :TODO: do we really need the whole graphics device id system and
	// getting devices by id?

	/////////////////////////////////////////////////////////////////////
	/// \brief Defines the interface for interacting with a GraphicsDevice
	/// of some kind
	/////////////////////////////////////////////////////////////////////
	class GraphicsDevice {
	private:
		u08                id;
	protected:
		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a new GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		GraphicsDevice();

		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a new GraphicsHandle for an object owned by this
		/// GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		template<u32 T_ID>
		_GraphicsHandle<T_ID> makeHandle(u32 id, u32 generation){
			_GraphicsHandle<T_ID> result;
			result._device     = this->id;
			result._generation = generation;
			result._id         = id;
			return result;
		}
	public:
		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys a GraphicsDevice and all associated resources
		/////////////////////////////////////////////////////////////////////
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
		/// \param mesh_geom   The mesh's geometry
		/// \param vertex_spec The vertex spec for the created mesh. The created
		/// mesh may rely on this memory, do not free while the mesh exists.
		///
	  /// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		///
		/// \todo :TODO: -> remove the limitation of created mesh relying on
		/// vertex_spec
		/////////////////////////////////////////////////////////////////////
		Mesh createMesh(const MeshGeometrySource& mesh_geom,
		                const VertexSpec&         vertex_spec);

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys a Mesh previously created by this GraphicsDevice
		/// freeing all associated hardware resources
		/////////////////////////////////////////////////////////////////////
		virtual void destroyMesh(Mesh mesh) = 0;
		/// @}

		// :TODO: have some way of updating an existing mesh for dynamic data
		// generated on the CPU

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

	GraphicsDevice* getGraphicsDevice(u08 id = 0);

	template<u32 T_ID>
	GraphicsDevice* getGraphicsDevice(_GraphicsHandle<T_ID> handle){
		return getGraphicsDevice(handle._device);
	}
}

#endif
