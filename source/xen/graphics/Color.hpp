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
#include <xen/math/vector.hpp>

namespace xen {

	typedef Vec3f Color3f;
	typedef Vec4f Color4f;

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

		static const Color   BLACK;
		static const Color   WHITE;
		static const Color   RED;
		static const Color   GREEN;
		static const Color   BLUE;
		static const Color   CYAN;
		static const Color   MAGENTA;
		static const Color   YELLOW;

		static const Color4f BLACK4f;
		static const Color4f WHITE4f;
		static const Color4f RED4f;
		static const Color4f GREEN4f;
		static const Color4f BLUE4f;
		static const Color4f CYAN4f;
		static const Color4f MAGENTA4f;
		static const Color4f YELLOW4f;

		Color();
		Color(const Color& other);
		Color(u08 r, u08 g, u08 b, u08 a = 255);
		Color(Color3f other, float a  = 1);
		Color(Color4f other);

		/// \brief Assigns this color to some other
		xen::Color& operator=(const xen::Color&   other);

		/// \brief Assigns this color to some other, leaving alpha unchanged
		xen::Color& operator=(const xen::Color3f& other);

		/// \brief Assigns this color to some other
		xen::Color& operator=(const xen::Color4f& other);

		explicit operator Color3f const();
		explicit operator Color4f const();
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
