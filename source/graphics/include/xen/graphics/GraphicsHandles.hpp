////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of various opaque types used to represent
/// handles to implementation specific types used by Xeno Engine's render
/// system
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GRAPHICSHANDLES_HPP
#define XEN_GRAPHICS_GRAPHICSHANDLES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

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

		/// \brief How many times this handle type and id has been reused
		u32 _generation;

	};

	template<typename T>
	T makeNullGraphicsHandle(){
		_GraphicsHandle<T::HANDLE_ID> result = {0};
		return result;
	}

	template<u32 T_ID>
	_GraphicsHandle<T_ID> makeGraphicsHandle(u32 id, u32 generation){
		_GraphicsHandle<T_ID> result;
		result._generation = generation;
		result._id         = id;
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

	/// \brief Handle to some texture which may be used by a GraphicsDevice
	typedef _GraphicsHandle<__LINE__> Texture;
}

#endif
