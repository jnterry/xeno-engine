////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of texture types and functions
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_TEXTURE_HXX
#define XEN_SREN_TEXTURE_HXX

#include <xen/graphics/Image.hpp>
#include <xen/graphics/Color.hpp>
#include <xen/math/vector_types.hpp>

namespace xen {
namespace sren {

struct TextureImpl {
	// :TODO: mipmaps
	// :TODO: texture wrapping/clamping/etc settings
	xen::RawImage image;
};

/////////////////////////////////////////////////////////////////////
/// \brief Samples a texture returning the color of the specified uv
/// coordinate
/////////////////////////////////////////////////////////////////////
Color4f sampleTexture(const TextureImpl*, Vec2f uv);

}
}

#endif
