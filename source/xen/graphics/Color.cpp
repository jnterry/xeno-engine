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
	const Color Color::BLACK   = xen::Color(   0,   0,   0);
	const Color Color::WHITE   = xen::Color( 255, 255, 255);
	const Color Color::RED     = xen::Color( 255,   0,   0);
	const Color Color::GREEN   = xen::Color(   0, 255,   0);
	const Color Color::BLUE    = xen::Color(   0,   0, 255);
	const Color Color::CYAN    = xen::Color(   0, 255, 255);
	const Color Color::MAGENTA = xen::Color( 255,   0, 255);
	const Color Color::YELLOW  = xen::Color( 255, 255,   0);

	xen::Color& xen::Color::operator=(u32 val) {
		this->value = val;
		return *this;
	}

	xen::Color::Color() {
		// no-op
	}
	xen::Color::Color(const Color& other)
		: r(other.r), b(other.b), g(other.g), a(other.a){
		// no-op
	}
	xen::Color::Color(u08 nr, u08 ng, u08 nb, u08 na)
		: r(nr), b(nb), g(ng), a(na){
		// no-op
	}
}

bool operator==(const xen::Color& lhs, const xen::Color& rhs) {
	return lhs.value == rhs.value;
}

bool operator!=(const xen::Color& lhs, const xen::Color& rhs){
	return lhs.value != rhs.value;
}

#endif
