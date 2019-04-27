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
		/// \note [0][0] is defined to be the BOTTOM LEFT corner of the image
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

	struct CubeMapUv {
		// \brief The UV offset within the face in question
		Vec2r uv;

		/// \brief Which face the pixel is on
		CubeMap::Face face;
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
			// y is swapped since when doing GL upload
			// we expect [0][0] of the array to be the TOP left
			// but we want to store with [0][0] as the BOTTOM left
			//
			// x is swapped since cubemaps are "indexed" by direction
			// from center of cube to the outside, hence left and
			// right are flipped when viewing from the inside
			return this->elements[
				((side_length * coord.z) +
				 (side_length - coord.y - 1)
				) * side_length +
				(side_length - coord.x - 1)
			];
		}
		const T& operator[](Vec3u coord) const {
			return this->elements[
				((side_length * coord.z) +
				 (side_length - coord.y - 1)
				) * side_length +
				(side_length - coord.x - 1)
			];
		}
	};
}

inline bool operator==(const xen::CubeMapUv& a, const xen::CubeMapUv& b){
	return a.uv == b.uv && a.face == b.face;
}

inline bool operator!=(const xen::CubeMapUv& a, const xen::CubeMapUv& b){
	return a.uv != b.uv || a.face != b.face;
}

#endif
