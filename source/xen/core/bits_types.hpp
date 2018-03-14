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
		/// \brief Pointer to the primitive the bit is contained within
		T* const  primitive;

		/// \brief Mask containing a single set bit, indicating which bit of the
		/// primitive this reference is for
		const T mask;

		operator bool() const {
			return *primitive & mask;
		}

		BitReference& operator=(bool val){
			*primitive &= ~mask;        // clear the bit to 0
			*primitive |= (mask * val); // set the bit to 1 iff val == true
			return *this;
		}

		/// \brief Sets the value of the bit to 0
		BitReference& clear(){
			*primitive &= ~mask;
			return *this;
		}

		/// \brief Sets the value of the bit to 1
		BitReference& set(){
			*primitive |= mask;
			return *this;
		}

		/// \brief Flips the value of the bit
		BitReference& toogle(){
			*primitive ^= mask;
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
		static const constexpr u32 NUM_PRIMITIVES = (T_NUM / (sizeof(T)*8) + 1);

		/// \brief Array of primitives holding the bit values
		T bits[NUM_PRIMITIVES];

		/// \brief Accesses the n'th bit of this bitfield in const manor
		const BitReference<T> operator[](u32 index) const;

		/// \brief Accesses the n'th but of this bitfield
		const BitReference<T> operator[](u32 index);

		/// \brief Determines if any bit of the bit field is set
	  operator bool(){
		  bool result = false;
		  for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
			  result |= (bool)bits[i];
		  }
		  return result;
	  }

		/// \brief Sets the low bits of this bit field to the specified value
		template<typename T2>
		BitField<T, T_NUM>& operator=(T2 val){
			bits[0] = val;
			return *this;
		}

		/// \brief Sets this bit field equal to some other
		BitField<T, T_NUM>& operator=(const BitField<T, T_NUM>& other){
			for(u32 i = 0; i < NUM_PRIMITIVES; ++i){
				this->bits[i] = other.bits[i];
			}
			return *this;
		}

		/// \brief No-op constructor
		BitField<T, T_NUM>(     ) { }

		/// \brief Sets the low bits of this bit field to the specified value
		template<typename T2>
		BitField<T, T_NUM>(T2 val) { bits[0] = val; }

		/// \brief Creates a new bit field that is deep copy of some other
		BitField<T, T_NUM>(const BitField<T, T_NUM>& other){
			*this = other;
		}
	};
}

template<typename T, u32 T_NUM>
bool operator==(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	bool equal = true;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
		equal &= (a.bits[i] == b.bits[i]);
	}
	return equal;
}
template<typename T, u32 T_NUM>
bool operator!=(const xen::BitField<T, T_NUM>& a, const xen::BitField<T, T_NUM>& b){
	bool nequal = false;
	for(u32 i = 0; i < xen::BitField<T, T_NUM>::NUM_PRIMITIVES; ++i){
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
