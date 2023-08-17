////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for loading, saving and maninpulating raw
/// images
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_IMAGE_HPP
#define XEN_GRAPHICS_IMAGE_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/math/angle.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/latlong.hpp>
#include <xen/graphics/Image_types.hpp>

namespace xen{
	struct ArenaLinear;
	class Allocator;

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new image of the specified size.
	///
	/// This function does not initialise the pixels of the image to any color
	///
	/// \note The created image should be passed to destroyImage in order to free
	/// the allocated memory
	///
	/// \param alloc The allocator with which to allocated the bytes to store
	/// the image
	/// \param size Size of the image to create in pixels
	/// \return Created raw image, will be cleared to 0 if allocation failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage createImage(Allocator& alloc, Vec2u size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new image of the specified size.
	///
	/// This function does not initialise the pixels of the image to any color
	///
	/// \note The pixel data for the created image will be stored in the specified
	/// ArenaLinear. No operation needs to be performed to free this memory
	///
	/// \param arena The arena to allocate the pixel data in
	/// \param size  The dimensions in pixels of the image to create
	/// \return Created raw image, will be cleared to 0 if allocation failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage createImage(ArenaLinear& arena, Vec2u size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an image from the specified file
	/// \param arena     Arena within with pixel data will be stored
	/// \param file_path Path to the file to load
	/// \return RawImage by value, will be cleared to 0 if load failed
	///
	/// /// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage loadImage(ArenaLinear& arena, const char* file_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Loads an image from the specified file
	///
	/// \note The created image should be passed to destroyImage in order to free
	/// the allocated memory
	///
	/// \param alloc     Allocator with which to allocate bytes to store pixel data
	/// \param file_path Path to the file to load
	/// \return RawImage by value, will be cleared to 0 if load failed
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	RawImage loadImage(Allocator& alloc, const char* file_path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Saves a RawImage to a file
	/// \param image The RawImage to save
	/// \param file_path The name of the file to save to, any existing file
	/// will be overwritten
	/// \param format The format of the image to save. Defaults to UNKNOWN, which
	/// means attempt to deduce from file extension. If file extension is unknown
	/// then function returns false.
	/// \return True if write succeeded, else false
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	bool saveImage(const RawImage& image, const char* file_path, ImageFormat format = ImageFormat::UNKNOWN);

	/////////////////////////////////////////////////////////////////////
	/// \brief Frees the memory allocated for the specified image.
	///
	/// \note This function should only be called if the image was created
	/// or loaded with the specified Allocator providing the memory for the
	/// image's pixels.
	/// \param alloc The allocator which allocated this image
	/// \param image The image to destroy
	///
	/// \public \memberof RawImage
	/////////////////////////////////////////////////////////////////////
	void destroyImage(Allocator& alloc, RawImage image);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the UV coordinates to sample from a cubemap
	/// given a direction from the center looking out towards the map
	/////////////////////////////////////////////////////////////////////
	CubeMapUv getCubeMapUv(Vec3r direction);
	inline CubeMapUv getCubeMapUv(LatLong latlong){
		return getCubeMapUv(xen::toCartesian(latlong));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the pixel of a cube map to be accessed given
	/// a direction from the center of the cube map towards one of the faces
	/// \return x and y component will range from 0 to face_size, z component
	/// will range from 0 to 5 to indicate which face should be sampled
	/// (see xen::CubeMap::Face)
	/////////////////////////////////////////////////////////////////////
	Vec3u getCubeMapPixelCoord(Vec3r direction, u32 face_size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the pixel of a cubemap to sample given a latitude and
	/// longitude
	/////////////////////////////////////////////////////////////////////
	Vec3u getCubeMapPixelCoord(LatLong latlong, u32 face_size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the latitude and longitude for the center of some
	/// pixel in a cube map
	/////////////////////////////////////////////////////////////////////
	LatLong getCubeMapLatLong(Vec3u cube_map_pixel, u32 face_size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes a direction vector from the center of a cube map to some
	/// pixel on its surface
	/// \param cube_map_pixel Coordinate of a cube map pixel, x and y are the pixel
	/// within the face. z represents which face
	/// \param face_size The dimensions of each face
	/////////////////////////////////////////////////////////////////////
	Vec3r getCubeMapDirection(Vec3u cube_map_pixel, u32 face_size);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves a direction vector from some center point outwards
	/// corresponding to a particular uv coordinate on a cubemap's surface
	/////////////////////////////////////////////////////////////////////
	Vec3r getCubeMapDirection(CubeMapUv uv);

	inline LatLong getCubeMapLatLong(xen::CubeMapUv coord){
		return xen::toLatLong(xen::getCubeMapDirection(coord));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the coordinate of the neighbour of some cubemap pixel
	/// in the specified direction
	///
	/// \note Traversing long distances across a cubemap's surface can lead to
	/// unexpected results since the "up", "left", etc directions are different
	/// on each face. In fact it is impossible to have a consistent direction
	/// (Consider placing a cube on a table such that the top face is "up",
	/// all sides now have their "up" pointing to the ceiling, however if you
	/// the "up" direction of one of these faces, and then continue across
	/// the top, the opposite vertical face's up will point in the opposite
	/// direction)
	/// This means that going LEFT and then immediately RIGHT might not get you
	/// back to the starting position!!!
	/// If you need to traverse long distances it is suggested that you use polar
	/// coordinates or similar to represent  motion on the surface of the unit
	/// sphere, and then use getCubeMapPixelCoord to map to a cubemap. The main
	/// use of this function is to enable blending/blurring across face boundaries
	/// but only by a single pixel!
	///
	/// Additionally while every pixel has an "up", "down", "left" and "right"
	/// neighbour, not all pixels will have, for example, and "up-left" neighbour
	/// (Consider a pixel in the very corner of some face -> there is no
	/// corresponding diagonal neighbour in one direction, since only 3 pixels
	/// meet at a vertex rather than 4)
	///
	/// \note If coord is outside the bounds of the face then this function will
	/// clamp the particular component (x for left/right or y for up/down) to
	/// be between 0 and face_size before computing the neighbour. This means that
	/// the return value is guaranteed to always be either on the same face as
	/// coord, or on the very edge of some other face.
  /////////////////////////////////////////////////////////////////////
	Vec3u getCubeMapPixelNeighbour(Vec3u coord, u32 face_size, xen::CubeMap::Direction dir);

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a weighted blend of cube map texels to be sampled
	/// in order to read the value of some CubeMapUv
	/////////////////////////////////////////////////////////////////////
	struct CubeMapSamplePoints {
		/// \brief The 4 pixels whose values must be blended together
		Vec3u coord[4];

		/// \brief The weightings for each pixel, sum of these will be equal to
		/// 1.0. Note that some of these may be 0.0 when less than 4 texels need
		/// to be sampled - for example if reading directly from the center of some
		/// pixel only 1 pixel needs be sampled. When reading directly from the
		/// corner of a cube face only 3 pixels need to be sampled since the
		/// curvature means there are only 3 pixels meeting at this vertex, not 4
		real  weight[4];
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes which texels need to be sampled and the corresponding
	/// blend weights in order to read the value of a cubemap at a location not
	/// directly centered on some pixel
	/// \return CubeMapSamplePoints representing the texels to be sampled. Arrays
	/// will be ordered such that:
	/// - [0] is the primary pixel that the uv falls within
	/// - [1] is the neighbour to the primary pixel stepping along the uv.x direction
	/// - [2] is the neighbour to the primary pixel stepping along the uv.y direction
	/// - [3] is diagonally touching the primary pixel, stepping along both uv.x and uv.y
	/////////////////////////////////////////////////////////////////////
	CubeMapSamplePoints getCubeMapSamplePoints(CubeMapUv uv, u32 face_size);

	inline CubeMapSamplePoints getCubeMapSamplePoints(Vec3r direction, u32 face_size){
		return getCubeMapSamplePoints(getCubeMapUv(direction), face_size);
	}
	inline CubeMapSamplePoints getCubeMapSamplePoints(LatLong latlong, u32 face_size){
		return getCubeMapSamplePoints(getCubeMapUv(latlong), face_size);
	}

	template<typename T>
	T sampleCubeArray(const CubeArray<T>& arr, LatLong ll){
		CubeMapSamplePoints pts = getCubeMapSamplePoints(ll, arr.side_length);

		/*bool bad = false;
		for(int i = 0; i < 4; ++i){
			bad |= pts.coord[i].x < 0;
			bad |= pts.coord[i].y < 0;
			bad |= pts.coord[i].z < 0;
			bad |= pts.coord[i].x >= arr.side_length  ;
			bad |= pts.coord[i].y >= arr.side_length  ;
			bad |= pts.coord[i].z >= 6;
		}
		if(bad){
			printf("BAD sample coord!\n");
			printf("%f, %f\n", xen::asDegrees(ll.x), xen::asDegrees(ll.y));
			for(int i = 0; i < 4; ++i){
				printf("[%i]: %u, %u, %u\n",
				       i,
				       pts.coord[i].x,
				       pts.coord[i].y,
				       pts.coord[i].z);
			}
			}*/

		// :TODO: this clamping is a nasty hack
		// Issue is sometimes getCubeMapSamplePoints return side_length
		// rather than side_length-1, usually when the latlong is right on the
		// boundary between two faces
		for(int i = 0; i < 4; ++i){
			pts.coord[i].x = xen::min(pts.coord[i].x, arr.side_length-1);
			pts.coord[i].y = xen::min(pts.coord[i].y, arr.side_length-1);
		}

		return (arr[pts.coord[0]] * pts.weight[0] +
		        arr[pts.coord[1]] * pts.weight[1] +
		        arr[pts.coord[2]] * pts.weight[2] +
		        arr[pts.coord[3]] * pts.weight[3]);
	}


	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the total length of the CubeArray's elements member
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	u32 size(const CubeArray<T>& array){
		return array.side_length * array.side_length * 6;
	}


	/////////////////////////////////////////////////////////////////////
	/// \brief Allocates a CubeArray with the specified side length
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	CubeArray<T> createCubeArray(xen::ArenaLinear& arena, u32 side_length){
		CubeArray<T> result;
		result.side_length = side_length;
		result.elements = xen::reserveTypeArray<T>(arena, xen::size(result));
		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Blurs the elements of a CubeArray with proper handling of
	/// wrapping accross faces
	/// \brief input - The input cubearray
	/// \brief rounds How many times to perform the blur operation
	/// \brief center_weight The weight given the tile actually being blured
	/// The new value after each round will be given by
	///  self_weight * current_value + ((1-self_weight)/4.0) * each_neighbour
	/// Defaults to 0.2, thus allowing the center tile and all neighbours to have
	/// an equal weighting
	/// \brief tmp_arena ArenaLinear used for temporary storage - will be rolled
	/// back by the end of this function. Needed to double buffer the source's
	/// contents, hence must have enough bytes free to create copy of source's
	/// elements
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	CubeArray<T>& blurCubeMap(CubeArray<T>& input,
	                            u32 num_rounds,
	                            xen::ArenaLinear& tmp_arena,
	                            float self_weight = 0.2){
		xen::MemoryTransaction transaction(tmp_arena); // do not commit this!

		CubeArray<T> double_buffer;
		double_buffer.side_length = input.side_length;
		double_buffer.elements    = xen::reserveTypeArray<T>(tmp_arena, xen::size(input));

		CubeArray<T>* source;
		CubeArray<T>* dest;
		if(num_rounds % 2 == 0){
			// If even number then start with input as will flip back, eg:
			// input --> double_buffer --> input
			source = &input;
			dest   = &double_buffer;
		} else {
			// If odd number then start with the double buffer so we end up copying
			// to input as final step, eg:
			// double_buffer --> input
			// This means we need to copy data accross to double buffer
			xen::copyArray(input.elements, double_buffer.elements, xen::size(input));
			source = &double_buffer;
			dest   = &input;
		}

		float neighbour_weight = (1.0 - self_weight) / 4.0;

		for(int i = 0; i < num_rounds; ++i){
			Vec3u pos;

			for(pos.z = 0; pos.z < 6; ++pos.z){
				for(pos.y = 0; pos.y < input.side_length; ++pos.y){
					for(pos.x = 0; pos.x < input.side_length; ++pos.x){

						(*dest)[pos] = self_weight * (*source)[pos];
						for(int dir = 0; dir < 4; ++dir){
							(*dest)[pos] += (
								neighbour_weight *
								(*source)[xen::getCubeMapPixelNeighbour(pos,
								                                        input.side_length,
								                                        (xen::CubeMap::Direction)dir
									)]
							);
						}
					}
				}
			}

			xen::swap(source, dest);

		}
	}


}

#endif
