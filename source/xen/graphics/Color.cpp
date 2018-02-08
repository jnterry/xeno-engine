////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of Color types/functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_COLOR_CPP
#define XEN_GRAPHICS_COLOR_CPP

#include <xen/graphics/Color.hpp>

namespace xen{
	const Color Color::BLACK = {   0,   0,   0,   255};
	const Color Color::WHITE = { 255, 255, 255,   255};
	const Color Color::RED   = { 255,   0,   0,   255};
	const Color Color::GREEN = {   0, 255,   0,   255};
	const Color Color::BLUE  = {   0,   0, 255,   255};

	xen::Color& xen::Color::operator=(u32 val) {
		this->value = val;
		return *this;
	}
}

bool operator==(const xen::Color& lhs, const xen::Color& rhs) {
	return lhs.value == rhs.value;
}

bool operator!=(const xen::Color& lhs, const xen::Color& rhs){
	return lhs.value != rhs.value;
}

#endif
