////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of Texture related functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_TEXTURE_CPP
#define XEN_SREN_TEXTURE_CPP

#include "Texture.hxx"

namespace xen {
namespace sren{

Color4f sampleTexture(const TextureImpl* texture, Vec2f uv){
	if(texture == nullptr){
		return xen::Color::WHITE4f;
	}

	if(uv.u < 0 || uv.v < 0 || uv.u > 1 || uv.v > 1){
		return xen::Color::BLACK4f;
	}

	Vec2u uv_int = (Vec2u)(uv * (Vec2r)(texture->image.size - Vec2u{1,1}));

	return xen::makeColor4f(texture->image[uv_int.u][uv_int.v]);
}

}
}

#endif
