////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of types for representing Textures - IE:
/// collections of pixels which may be used by the graphics backend while
/// rendering
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_TEXTURE_TYPES_HPP
#define XEN_GRAPHICS_TEXTURE_TYPES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{
	struct Texture {
		enum Type {
			/// \brief A single 2d plane
			Plane,

			/// \brief 6 2d square textures of the same size representing
			/// the 6 faces of a cube. Can be sampled using 3d direction vector
			/// from the center of the cube
			CubeMap,
		};

		/// \brief The type of texture represented
		Type type;

		/// \brief The size of the texture in pixels
		/// For Plane textures only x and y is used
		/// For cubemaps, this is the size of each face
		Vec3u size;

		/// \brief The number of channels in the texture, may be between 1 and 4
		/// with the following semantics:
		/// 1 -> grayscale, a assumed to be 1
		/// 2 -> grayscale with alpha
		/// 3 -> rgb, a assumed to be 1
		/// 4 -> rgb with alpha
		u08  channels;

		/// \brief If true then each each channel of each pixel is represented
		/// by a 32bit float, otherwise a 8 bit unsigned byte is used
		bool is_floating;
	};
}

#endif
