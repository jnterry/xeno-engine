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

namespace xen {
	struct MeshData;

	enum class GraphicsDeviceType {
		Null,
		Software,
		OpenGL,
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Defines the interface for interacting with a GraphicsDevice
	/// of some kind
	/////////////////////////////////////////////////////////////////////
	class GraphicsDevice {
	private:
		/// \brief The type of this GraphicsDevice
		GraphicsDeviceType type;
		u08                id;
	protected:
		/////////////////////////////////////////////////////////////////////
		/// \brief Creates a new GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		GraphicsDevice(GraphicsDeviceType type);

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
		/// \brief Retreives the GraphicsDeviceType of this GraphicsDevice
		/////////////////////////////////////////////////////////////////////
		GraphicsDeviceType getDeviceType();

		/////////////////////////////////////////////////////////////////////
		/// \brief Destroys a GraphicsDevice and all associated resources
		/////////////////////////////////////////////////////////////////////
		virtual ~GraphicsDevice();

		// :TODO: create render target
		// :TDOO: create window

		/// \defgroup Mesh
		/// @{
		/////////////////////////////////////////////////////////////////////
		/// \brief Uploads mesh data to the graphics device
		/// \return Mesh Handle to the created mesh, this handle may be used
		/// in future with this GraphicsDevice to render the mesh
		/////////////////////////////////////////////////////////////////////
		virtual Mesh createMesh(MeshData& mesh_data) = 0;

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
		/// \brief Clears a region of a render target to some color
		/////////////////////////////////////////////////////////////////////
		virtual void clear(RenderTarget target,
		                   const xen::Aabb2u& viewport,
		                   xen::Color color
		                  ) = 0;

		/////////////////////////////////////////////////////////////////////
		/// \brief Renders a series of render commands to some viewport of
		/// some RenderTarget
		/////////////////////////////////////////////////////////////////////
		virtual void render(RenderTarget target,
		                    const xen::Aabb2u& viewport,
		                    const RenderParameters3d& params,
		                    const xen::Array<RenderCommand3d> commands
		                   ) = 0;
		/// @}
	};

	GraphicsDevice* getGraphicsDevice(u08 id = 0);

	template<u32 T_ID>
	GraphicsDevice* getGraphicsDevice(_GraphicsHandle<T_ID> handle){
		return getGraphicsDevice(handle._device);
	}
}

#endif
