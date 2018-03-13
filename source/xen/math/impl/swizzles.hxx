////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains fundamental types for xen's swizzle operators
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_SWIZZLE_HXX
#define XEN_MATH_SWIZZLE_HXX

namespace xen {
	namespace impl {
		/////////////////////////////////////////////////////////////////////
		/// \brief Type which maps from characters to the index of a vector
		/// containing that component at compile time
		/////////////////////////////////////////////////////////////////////
		template<char T_VAL>
		struct SwizzlePlace {
			// We do bounds checking, so set to max value so compile errors are
			// generated if we ever use this non-specialized SwizzlePlace
			static const constexpr u32 INDEX = 0xFFFFFFFF;
		};

		template<> struct SwizzlePlace<'x'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'y'>{ static const constexpr u32 INDEX = 1; };
		template<> struct SwizzlePlace<'z'>{ static const constexpr u32 INDEX = 2; };
		template<> struct SwizzlePlace<'w'>{ static const constexpr u32 INDEX = 3; };

		template<> struct SwizzlePlace<'X'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'Y'>{ static const constexpr u32 INDEX = 1; };
		template<> struct SwizzlePlace<'Z'>{ static const constexpr u32 INDEX = 2; };
		template<> struct SwizzlePlace<'W'>{ static const constexpr u32 INDEX = 3; };

		template<> struct SwizzlePlace<'r'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'g'>{ static const constexpr u32 INDEX = 1; };
		template<> struct SwizzlePlace<'b'>{ static const constexpr u32 INDEX = 2; };
		template<> struct SwizzlePlace<'a'>{ static const constexpr u32 INDEX = 3; };

		template<> struct SwizzlePlace<'R'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'G'>{ static const constexpr u32 INDEX = 1; };
		template<> struct SwizzlePlace<'B'>{ static const constexpr u32 INDEX = 2; };
		template<> struct SwizzlePlace<'A'>{ static const constexpr u32 INDEX = 3; };

		template<> struct SwizzlePlace<'u'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'v'>{ static const constexpr u32 INDEX = 1; };

		template<> struct SwizzlePlace<'U'>{ static const constexpr u32 INDEX = 0; };
		template<> struct SwizzlePlace<'V'>{ static const constexpr u32 INDEX = 1; };
	}
}

#endif
