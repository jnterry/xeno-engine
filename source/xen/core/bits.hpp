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

namespace xen {

	template<typename T>
	const xen::BitReference<T> makeBitReference(const T* primative, u32 index){
		return {
			const_cast<T*>(&primative[index / (sizeof(T)*8)]),
			(T)1 << (index % (sizeof(T)*8)),
		};
	}

	template<typename T>
	xen::BitReference<T> makeBitReference(T* primative, u32 index){
		return {
			&primative[index / (sizeof(T)*8)],
			(T)1 <<   (index % (sizeof(T)*8)),
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

template<typename T>
bool operator!(xen::BitReference<T>& a){
	return !((bool)a);
}

template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator|(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		result.bits[i] = a.bits[i] | b.bits[i];
	}
	return result;
}
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator&(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		result.bits[i] = a.bits[i] & b.bits[i];
	}
	return result;
}
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator^(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	xen::BitField<T, T_NUM> result;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		result.bits[i] = a.bits[i] ^ b.bits[i];
	}
	return result;
}


template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM>& operator|=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		a.bits[i] |= b.bits[i];
	}
	return a;
}
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator&=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		a.bits[i] &= b.bits[i];
	}
	return a;
}
template<typename T, u32 T_NUM>
xen::BitField<T, T_NUM> operator^=(xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		a.bits[i] ^= b.bits[i];
	}
	return a;
}

#endif
