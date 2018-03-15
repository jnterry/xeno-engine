////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_BITS_HPP
#define XEN_CORE_BITS_HPP

#include <xen/core/bits_types.hpp>
#include <type_traits>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new BitReference
	/// \constructor
	/// \public \memberof BitReference
	/// \param primitive Pointer to the primitive containing the bit in question
	/// \param index The index of the bit within the primitive in question
	/// \note index may exceed the number of bits type T, in this case it is
	/// assume primitive is a pointer to the first element of an array of type T.
	/// The created reference will hence refer to a bit in the appropriate element
	/// of that array.
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	const xen::BitReference<T> makeBitReference(const T* primitive, u32 index){
		return {
			const_cast<T*>(&primitive[index / (sizeof(T)*8)]),
			(T)1 << (index % (sizeof(T)*8)),
		};
	}

	template<typename T>
	xen::BitReference<T> makeBitReference(T* primitive, u32 index){
		return {
			&primitive[index / (sizeof(T)*8)],
			(T)(1 <<  (index % (sizeof(T)*8))),
		};
	}

	template<typename T, u32 T_NUM>
	const BitReference<T> BitField<T, T_NUM>::operator[](u32 index) const {
		return makeBitReference(bits, index);
	}

	template<typename T, u32 T_NUM>
	const BitReference<T> BitField<T, T_NUM>::operator[](u32 index){
		return makeBitReference(bits, index);
	}
}

/// \brief Returns the not of the value of the bit referenced by this BitReference
/// \public \memberof xen::BitReference
template<typename T>
bool operator!(xen::BitReference<T>& a){
	return !((bool)a);
}

/// \brief Determines if all bits are set to 0 in some bit field
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
bool operator!(xen::BitField<T, T_NUM>& a){
	return !((bool)a);
}

/// \brief Computes bitwise or of two bit fields
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator|(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		result.bits[i] = a.bits[i] | b.bits[i];
	}
	return result;
}

/// \brief Computes bitwise or of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator|(const xen::BitField<T, T_NUM>& a, T b){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] |= b;
	return result;
}

/// \brief Computes bitwise or of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator|(T b, const xen::BitField<T, T_NUM>& a){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] |= b;
	return result;
}

/// \brief Computes bitwise and of two bit fields
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator&(const xen::BitField<T, T_NUM>& a,
                                  const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		result.bits[i] = a.bits[i] & b.bits[i];
	}
	return result;
}

/// \brief Computes bitwise and of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator&(const xen::BitField<T, T_NUM>& a, T2 b){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] &= b;
	return result;
}

/// \brief Computes bitwise and of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator&(T2 b, const xen::BitField<T, T_NUM>& a){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] &= b;
	return result;
}

/// \brief Computes bitwise xor of two bit fields
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator^(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		result.bits[i] = a.bits[i] ^ b.bits[i];
	}
	return result;
}

/// \brief Computes bitwise xor of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator^(const xen::BitField<T, T_NUM>& a, T2 b){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] ^= b;
	return result;
}

/// \brief Computes bitwise xor of a bitfield's lower bits and some primitive
/// \public \memberof xen::BitField
///
/// \note This function is enabled iff T2 is a scalar type.
/// This is to prevent ambiguity with T2 being equal to xen::BitField<T, T_NUM>
/// in the case where we try to and together two bit fields
template<typename T, u32 T_NUM, typename T2>
typename std::enable_if<std::is_scalar<T2>::value, xen::BitField<T, T_NUM> >::type
operator^(T2 b, const xen::BitField<T, T_NUM>& a){
	xen::BitField<T, T_NUM> result(a);
	result.bits[0] ^= b;
	return result;
}


/// \brief Computes bitwise not of a bit field
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator~(const xen::BitField<T, T_NUM>& a){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		result.bits[i] = ~a.bits[i];
	}
	return result;
}


/// \brief Sets LHS to the bitwise or of itself and RHS BitField
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM>& operator|=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		a.bits[i] |= b.bits[i];
	}
	return a;
}

/// \brief Sets LHS to the bitwise or the bitfield's lower bits and RHS
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM, typename T2>
xen::BitField<T, T_NUM>& operator|=(xen::BitField<T, T_NUM>& a, T2 b){
	a.bits[0] |= b;
	return a;
}

/// \brief Sets LHS to the bitwise and of itself and RHS BitField
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator&=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		a.bits[i] &= b.bits[i];
	}
	return a;
}

/// \brief Sets LHS to the bitwise and the bitfield's lower bits and RHS
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM, typename T2>
xen::BitField<T, T_NUM>& operator&=(xen::BitField<T, T_NUM>& a, T2 b){
	a.bits[0] &= b;
	return a;
}

/// \brief Sets LHS to the bitwise xor of itself and RHS BitField
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator^=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		a.bits[i] ^= b.bits[i];
	}
	return a;
}

/// \brief Sets LHS to the bitwise xor the bitfield's lower bits and RHS
/// \public \memberof xen::BitField
template<typename T, u32 T_NUM, typename T2>
xen::BitField<T, T_NUM>& operator^=(xen::BitField<T, T_NUM>& a, T2 b){
	a.bits[0] ^= b;
	return a;
}

#endif
