////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for representing and manipulating colors
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_COLOR_HPP
#define XEN_GRAPHICS_COLOR_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	// gcc doesn't like the anonymous structures inside unions, disable the warning temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	/////////////////////////////////////////////////////////////////////
	/// \brief Color represented as 8 bits per channel Rgba
	/////////////////////////////////////////////////////////////////////
	struct Color{
		union{
			struct { u08 r,g,b,a; };
			u32 value;
		};

		static const Color BLACK;
		static const Color WHITE;
		static const Color RED;
		static const Color GREEN;
		static const Color BLUE;
		static const Color CYAN;
		static const Color MAGENTA;
		static const Color YELLOW;

		Color();
		Color(const Color& other);
		Color(u08 r, u08 g, u08 b, u08 a = 255);

		xen::Color& operator=(u32 val);
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Color represented as 4 floating values between 0 and 1
	/////////////////////////////////////////////////////////////////////
	//struct Color4f {
	//	float r, g, b, a;
	//};
}

bool operator==(const xen::Color& lhs, const xen::Color& rhs);
bool operator!=(const xen::Color& lhs, const xen::Color& rhs);

#endif
