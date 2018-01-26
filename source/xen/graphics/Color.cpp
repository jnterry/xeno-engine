////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \file Color.cpp
/// \author Jamie Terry
/// \date 2018/01/25
/// \brief Contains definition of Color types/functions
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_COLOR_CPP
#define XEN_GRAPHICS_COLOR_CPP

#include <xen/graphics/Color.hpp>

namespace xen{
	const Color Color::BLACK = {   0,   0,   0,   0};
	const Color Color::WHITE = { 255, 255, 255,   0};
	const Color Color::RED   = { 255,   0,   0,   0};
	const Color Color::GREEN = {   0, 255,   0,   0};
	const Color Color::BLUE  = {   0,   0, 255,   0};
}

#endif
