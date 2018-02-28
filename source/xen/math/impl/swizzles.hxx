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
	template<u32 T_VAL>
	struct SwizzlePlace {
		static const constexpr u32 INDEX = T_VAL;
	};

	/// \brief Dummy type used to represent a swizzle of the x coordinate of a vector
	typedef SwizzlePlace<0> X;
	typedef SwizzlePlace<1> Y;
	typedef SwizzlePlace<2> Z;
	typedef SwizzlePlace<3> W;

	typedef SwizzlePlace<0> R;
	typedef SwizzlePlace<1> G;
	typedef SwizzlePlace<2> B;
	typedef SwizzlePlace<3> A;

	typedef SwizzlePlace<0> U;
	typedef SwizzlePlace<1> V;
}

#endif
