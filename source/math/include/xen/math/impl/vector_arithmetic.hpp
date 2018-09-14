////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains basic arithmetic operations which operate on vectors
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_IMPL_VECTOR_ARITHMETIC_HPP
#define XEN_MATH_IMPL_VECTOR_ARITHMETIC_HPP

#include <xen/math/vector_types.hpp>

template<typename T>
xen::Vec<2,T> operator+=(xen::Vec<2,T>& lhs, const xen::Vec<2, T>& rhs){
	lhs.x += rhs.x;
	lhs.y += rhs.y;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator+=(xen::Vec<3,T>& lhs, const xen::Vec<3, T>& rhs){
	lhs.x += rhs.x;
	lhs.y += rhs.y;
	lhs.z += rhs.z;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator+=(xen::Vec<4,T>& lhs, const xen::Vec<4, T>& rhs){
	lhs.x += rhs.x;
	lhs.y += rhs.y;
	lhs.z += rhs.z;
	lhs.w += rhs.w;
	return lhs;
}
template<u32 T_DIM, typename T>
xen::Vec<T_DIM, T> operator+(const xen::Vec<T_DIM,T>& lhs, const xen::Vec<T_DIM,T>& rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result += rhs;
	return result;
}



template<typename T>
xen::Vec<2,T> operator-=(xen::Vec<2,T>& lhs, const xen::Vec<2, T>& rhs){
	lhs.x -= rhs.x;
	lhs.y -= rhs.y;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator-=(xen::Vec<3,T>& lhs, const xen::Vec<3, T>& rhs){
	lhs.x -= rhs.x;
	lhs.y -= rhs.y;
	lhs.z -= rhs.z;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator-=(xen::Vec<4,T>& lhs, const xen::Vec<4, T>& rhs){
	lhs.x -= rhs.x;
	lhs.y -= rhs.y;
	lhs.z -= rhs.z;
	lhs.w -= rhs.w;
	return lhs;
}
template<u32 T_DIM, typename T>
xen::Vec<T_DIM, T> operator-(const xen::Vec<T_DIM,T>& lhs, const xen::Vec<T_DIM,T>& rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result -= rhs;
	return result;
}



template<typename T>
xen::Vec<2,T> operator*=(xen::Vec<2,T>& lhs, const xen::Vec<2, T>& rhs){
	lhs.x *= rhs.x;
	lhs.y *= rhs.y;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator*=(xen::Vec<3,T>& lhs, const xen::Vec<3, T>& rhs){
	lhs.x *= rhs.x;
	lhs.y *= rhs.y;
	lhs.z *= rhs.z;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator*=(xen::Vec<4,T>& lhs, const xen::Vec<4, T>& rhs){
	lhs.x *= rhs.x;
	lhs.y *= rhs.y;
	lhs.z *= rhs.z;
	lhs.w *= rhs.w;
	return lhs;
}
template<typename T>
xen::Vec<2,T> operator*=(xen::Vec<2,T>& lhs, T rhs){
	lhs.x *= rhs;
	lhs.y *= rhs;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator*=(xen::Vec<3,T>& lhs, T rhs){
	lhs.x *= rhs;
	lhs.y *= rhs;
	lhs.z *= rhs;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator*=(xen::Vec<4,T>& lhs, T rhs){
	lhs.x *= rhs;
	lhs.y *= rhs;
	lhs.z *= rhs;
	lhs.w *= rhs;
	return lhs;
}
template<u32 T_DIM, typename T>
xen::Vec<T_DIM, T> operator*(const xen::Vec<T_DIM,T>& lhs, const xen::Vec<T_DIM,T>& rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result *= rhs;
	return result;
}
template<u32 T_DIM, typename T, typename T_SCALAR>
xen::Vec<T_DIM, T> operator*(const xen::Vec<T_DIM,T>& lhs, T_SCALAR rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result *= (T)rhs;
	return result;
}
template<u32 T_DIM, typename T, typename T_SCALAR>
xen::Vec<T_DIM, T> operator*(T_SCALAR lhs, const xen::Vec<T_DIM,T>& rhs){
	xen::Vec<T_DIM, T> result = rhs;
	result *= (T)lhs;
	return result;
}


template<typename T>
xen::Vec<2,T> operator/=(xen::Vec<2,T>& lhs, const xen::Vec<2, T>& rhs){
	lhs.x /= rhs.x;
	lhs.y /= rhs.y;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator/=(xen::Vec<3,T>& lhs, const xen::Vec<3, T>& rhs){
	lhs.x /= rhs.x;
	lhs.y /= rhs.y;
	lhs.z /= rhs.z;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator/=(xen::Vec<4,T>& lhs, const xen::Vec<4, T>& rhs){
	lhs.x /= rhs.x;
	lhs.y /= rhs.y;
	lhs.z /= rhs.z;
	lhs.w /= rhs.w;
	return lhs;
}
template<typename T>
xen::Vec<2,T> operator/=(xen::Vec<2,T>& lhs, T rhs){
	lhs.x /= rhs;
	lhs.y /= rhs;
	return lhs;
}
template<typename T>
xen::Vec<3,T> operator/=(xen::Vec<3,T>& lhs, T rhs){
	lhs.x /= rhs;
	lhs.y /= rhs;
	lhs.z /= rhs;
	return lhs;
}
template<typename T>
xen::Vec<4,T> operator/=(xen::Vec<4,T>& lhs, T rhs){
	lhs.x /= rhs;
	lhs.y /= rhs;
	lhs.z /= rhs;
	lhs.w /= rhs;
	return lhs;
}
template<u32 T_DIM, typename T>
xen::Vec<T_DIM, T> operator/(const xen::Vec<T_DIM,T>& lhs, const xen::Vec<T_DIM,T>& rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result /= rhs;
	return result;
}
template<u32 T_DIM, typename T>
xen::Vec<T_DIM, T> operator/(const xen::Vec<T_DIM,T>& lhs, T rhs){
	xen::Vec<T_DIM, T> result = lhs;
	result /= rhs;
	return result;
}

template<typename T>
xen::Vec<2, T> operator-(const xen::Vec<2,T>& vec){
	return {-vec.x, -vec.y};
}
template<typename T>
xen::Vec<3, T> operator-(const xen::Vec<3,T>& vec){
	return {-vec.x, -vec.y, -vec.z};
}
template<typename T>
xen::Vec<4, T> operator-(const xen::Vec<4,T>& vec){
	return {-vec.x, -vec.y, -vec.z, -vec.w};
}

#endif
