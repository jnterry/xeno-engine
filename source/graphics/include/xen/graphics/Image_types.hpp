////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for loading, saving and maninpulating raw
/// images
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_IMAGE_TYPES_HPP
#define XEN_GRAPHICS_IMAGE_TYPES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	struct ArenaLinear;
	class Allocator;

	// gcc doesn't like the anonymous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents raw image data stored in main memory
	///
	/// \todo :TODO:Replace with Array2d typedefed -> we loose the .size
	/// (although ideally we would add that to Array2d as well)
	/////////////////////////////////////////////////////////////////////
	struct RawImage{
		union{
			struct{ u32 width, height; };
			Vec2u size;
		};
		/// \brief Array of length width*height holding color of each pixel
		Color* pixels;

		/////////////////////////////////////////////////////////////////////
		/// \brief Helper struct used to access pixels of the image
		/////////////////////////////////////////////////////////////////////
		struct ColRef {
			RawImage& image;
			u32       col;

			Color&       operator[](u32 row);
			const Color& operator[](u32 row) const;
		};

		const ColRef operator[](u32 col) const;
		ColRef       operator[](u32 col);
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	enum class ImageFormat {
		PNG,
		BMP,
		TGA,
		JPG,
		UNKNOWN,
	};


	struct CubeMap {
		enum Face {
			// DO NOT REARRANGE THESE
			// Graphics backend and Image.cpp functions rely on the order!!!
			PositiveX,
			PositiveY,
			PositiveZ,
			NegativeX,
			NegativeY,
			NegativeZ,
		};

		enum Direction {
			Down,
			Right,
			Up,
			Left,
		};
	};

	/// \brief Array representing data stored upon a CubeMap surface
	template<typename T>
	struct CubeArray {
		/// \brief The length of a single side of the CubeArray
		u32 side_length;

		/// \brief Pointer to first element of the data
		/// Use operator[] to access correct offset
		T* elements;

		inline T& operator[](Vec3u coord){
			return this->elements[side_length * side_length * coord.z +
			                      side_length * coord.y +
			                      coord.x];
		}
		const T& operator[](Vec3u coord) const {
			return this->elements[side_length * side_length * coord.z +
			                      side_length * coord.y +
			                      coord.x];
		}
	};
}

#endif
