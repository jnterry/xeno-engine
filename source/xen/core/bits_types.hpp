////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Defines useful functions for manipulating bits, and base class for
/// bit fields
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_BITS_TYPES_HPP
#define XEN_CORE_BITS_TYPES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a reference to some bit which can be manipulated
	/// as if it were a bool
	/// \tparam T The type of primitive the bit is contained within
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	struct BitReference {
		/// \brief Pointer to the primative the bit is contained within
		T* const  primative;

		/// \brief Mask containing a single set bit, indicating which bit of the
		/// primitive this reference is for
		const T mask;

		operator bool() const {
			return *primative & mask;
		}

		BitReference& operator=(bool val){
			*primative &= ~mask;        // clear the bit to 0
			*primative |= (mask * val); // set the bit to 1 iff val == true
			return *this;
		}

		/// \brief Sets the value of the bit to 0
		BitReference& clear(){
			*primative &= ~mask;
			return *this;
		}

		/// \brief Sets the value of the bit to 1
		BitReference& set(){
			*primative |= mask;
			return *this;
		}

		/// \brief Flips the value of the bit
		BitReference& toogle(){
			*primative ^= mask;
			return *this;
		}
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a set of bits of some size
	/// \tparam T     The underlying type used to store bits
	/// \tparam T_NUM The number of bits in this bit field
	/////////////////////////////////////////////////////////////////////
	template<typename T, u32 T_NUM>
	struct BitField {
		/// \brief The number of bits this BitField can hold
		static const constexpr u32 NUM_BITS       = T_NUM;

		/// \brief The number of primitives of type T used to hold the bits
		/// :TODO: don't need the +1 if T_NUM is exact multiple of sizeof(T)*8
		static const constexpr u32 NUM_PRIMATIVES = (T_NUM / (sizeof(T)*8) + 1);

		T bits[NUM_PRIMATIVES];

		const BitReference<T> operator[](u32 index) const;
		const BitReference<T> operator[](u32 index);
	};
}

template<typename T, u32 T_NUM>
bool operator==(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	bool equal = true;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		equal &= (a.bits[i] == b.bits[i]);
	}
	return equal;
}
template<typename T, u32 T_NUM>
bool operator!=(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	bool nequal = false;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMATIVES; ++i){
		nequal |= (a.bits[i] != b.bits[i]);
	}
	return nequal;
}

template<typename T>
bool operator==(const xen::BitReference<T>& a, const xen::BitReference<T>& b){
	return (bool)a == (bool)b;
}
template<typename T>
bool operator!=(const xen::BitReference<T>& a, const xen::BitReference<T>& b){
	return (bool)a != (bool)b;
}

#endif
