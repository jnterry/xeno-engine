////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of Color types/functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_COLOR_CPP
#define XEN_GRAPHICS_COLOR_CPP

#include <xen/math/utilities.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{
	const Color   Color::BLACK     = xen::Color(   0,   0,   0);
	const Color   Color::WHITE     = xen::Color( 255, 255, 255);
	const Color   Color::RED       = xen::Color( 255,   0,   0);
	const Color   Color::GREEN     = xen::Color(   0, 255,   0);
	const Color   Color::BLUE      = xen::Color(   0,   0, 255);
	const Color   Color::CYAN      = xen::Color(   0, 255, 255);
	const Color   Color::MAGENTA   = xen::Color( 255,   0, 255);
	const Color   Color::YELLOW    = xen::Color( 255, 255,   0);

	const Color4f Color::BLACK4f   = { 0.0f, 0.0f, 0.0f, 1.0f };
	const Color4f Color::WHITE4f   = { 1.0f, 1.0f, 1.0f, 1.0f };
	const Color4f Color::RED4f     = { 1.0f, 0.0f, 0.0f, 1.0f };
	const Color4f Color::GREEN4f   = { 0.0f, 1.0f, 0.0f, 1.0f };
	const Color4f Color::BLUE4f    = { 0.0f, 0.0f, 1.0f, 1.0f };
	const Color4f Color::CYAN4f    = { 0.0f, 1.0f, 1.0f, 1.0f };
	const Color4f Color::MAGENTA4f = { 1.0f, 0.0f, 1.0f, 1.0f };
	const Color4f Color::YELLOW4f  = { 1.0f, 1.0f, 0.0f, 1.0f };

	xen::Color& xen::Color::operator=(const xen::Color& other) {
		this->value = other.value;
		return *this;
	}

	xen::Color& xen::Color::operator=(const xen::Color3f& other){
		this->r = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.r);
		this->g = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.g);
		this->b = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.b);
		return *this;
	}

	xen::Color& xen::Color::operator=(const xen::Color4f& other){
		this->r = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.r);
		this->g = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.g);
		this->b = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.b);
		this->a = (u08)xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, other.a);
		return *this;
	}

	xen::Color::operator Color3f const(){
		return {
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->r),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->g),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->b)
		};
	}

	xen::Color::operator Color4f const(){
		return {
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->r),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->g),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->b),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, (float)this->a)
		};
	}

	xen::Color::Color() {
		// no-op
	}
	xen::Color::Color(const Color& other)
		: r(other.r), g(other.g), b(other.b), a(other.a){
		// no-op
	}
	xen::Color::Color(u08 nr, u08 ng, u08 nb, u08 na)
		: r(nr), g(ng), b(nb), a(na){
		// no-op
	}

	Color::Color(Color3f other, float a) {
		*this = xen::mkVec(other, a);
	}
	Color::Color(Color4f other) {
		*this = other;
	}
}

bool operator==(const xen::Color& lhs, const xen::Color& rhs) {
	return lhs.value == rhs.value;
}

bool operator!=(const xen::Color& lhs, const xen::Color& rhs){
	return lhs.value != rhs.value;
}

#endif
