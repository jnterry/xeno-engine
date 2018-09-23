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
	const Color   Color::BLACK     = {    0,   0,   0, 255 };
	const Color   Color::WHITE     = {  255, 255, 255, 255 };
	const Color   Color::RED       = {  255,   0,   0, 255 };
	const Color   Color::GREEN     = {    0, 255,   0, 255 };
	const Color   Color::BLUE      = {    0,   0, 255, 255 };
	const Color   Color::CYAN      = {    0, 255, 255, 255 };
	const Color   Color::MAGENTA   = {  255,   0, 255, 255 };
	const Color   Color::YELLOW    = {  255, 255,   0, 255 };

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

	Color4f makeColor4f(Color color) {
		return (Color4f)color;
	}
	Color4f makeColor4f(Color3f c, float a){
		return {
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, c.r),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, c.g),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, c.b),
			xen::mapToRangeClamped<u08, float>(0, 255, 0.0f, 1.0f, a)
		};
	}


	Color makeColor(Color3f c){
		return {
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.r),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.g),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.b),
			255
		};
	}
	Color makeColor(Color4f c){
		return {
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.r),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.g),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.b),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.a),
		};
	}
	Color makeColor(Color3f c, float a){
		return {
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.r),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.g),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255, c.b),
			xen::mapToRangeClamped<float, u08>(0.0f, 1.0f, 0, 255,   a),
		};
	}
}

bool operator==(const xen::Color& lhs, const xen::Color& rhs) {
	return lhs.value == rhs.value;
}

bool operator!=(const xen::Color& lhs, const xen::Color& rhs){
	return lhs.value != rhs.value;
}

#endif
