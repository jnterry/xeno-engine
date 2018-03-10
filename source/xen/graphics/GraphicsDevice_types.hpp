////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of various opaque types used to represent
/// handles to implementation specific types used by Xeno Engine's render
/// system
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSDEVICE_TYPES_HPP
#define XEN_GRAPHICS_GRAPHICSDEVICE_TYPES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	/// \brief Opaque type representing a entry point into xeno engine's rendering
	/// system. In short, the GraphicsDevice is responsible for managing some set
	/// of  hardware resources in order to facilitate rendering operations.
	///
	/// Xeno Engine supports the creation of multiple graphics devices
	/// simultaneously, although in most applications only one will be required.
	/// Note that the number supported is low.
	///
	/// One possible use case for creating multiple devices is to create one
	/// backed by the GPU and another backed by a software only renderer.
	/// This could allow for better use of compute resources if we can use the
	/// GPU to 100% without using 100% of the CPU.
	/// Additionally, it could allow for the GPU to perform real time rendering
	/// for a game, while rendering higher fidelity raytraced images on the CPU
	/// which may then be presented as a part of the real time application.
	struct GraphicsDevice;

	/// \brief Handle type which refers to some object owned by a GraphicsDevice
	///
	/// This should be treated as an opaque type! It may be copied, passed by value, etc
	///
	/// \note This type is templated such that graphics handles for one type of
	/// resources are a different type to those for some other type of resource to
	/// allow for strict type checking even thought the handles are really just an integer.
	template <u32 T_ID>
	struct _GraphicsHandle {
		static constexpr const u32 HANDLE_ID = T_ID;

		/// \brief The id of the object referred to by this handle
		u32 _id;

		u32
			/// \brief How many times this handle type and id has been reused
			_generation : 24,
			/// \brief The device which owns the resource referred to by this handle
			_device     :  8;
	};

	template<typename T>
	T makeNullHandle(){
		_GraphicsHandle<T::HANDLE_ID> result = {0};
		return result;
	}

	/// \brief Handle to a Mesh which may be drawn by some GraphicsDevice.
	///
	/// A Mesh is the fundamental unit of geometry that may be rendered
	/// by a GraphicsDevice. Meshes consist of some number vertices each
	/// with a position and optionally some other associated attributes,
	/// such as color, normal, uv coordiates, etc
	typedef _GraphicsHandle<__LINE__> Mesh;

	/// \brief Handle to some surface which may be drawn to by a GraphicsDevice.
	///
	/// A RenderTarget will have a color surface representing the colors of pixels
	/// on the surface, and may have other surfaces such as a depth buffer, etc
	typedef _GraphicsHandle<__LINE__> RenderTarget;
}

#endif
