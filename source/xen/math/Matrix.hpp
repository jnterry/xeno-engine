////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Matrix.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains type and functions for manipulating matricies
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_MATRIX_HPP
#define XEN_MATH_MATRIX_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/Vector.hpp>

// gcc doesn't like the anonomous structures inside unions,
// disable the warning in this file
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

namespace xen{
	template<u32 T_Rows, u32 T_Cols, typename T>
	struct Matrix{
		union{
			T elements[T_Rows][T_Cols];
			Vec<T_Cols, T> rows[T_Rows]; //:TODO: not sure if in row major or column major format yet...
		};
	};
}

#pragma GCC diagnostic pop // re-enable -Wpedantic

template<typename T> using Mat3 = xen::Matrix<3,3, T>;
template<typename T> using Mat4 = xen::Matrix<4,4, T>;

typedef Mat3<r32>  Mat3f;
typedef Mat3<r64>  Mat3d;
typedef Mat3<real> Mat3r;
typedef Mat3<u32>  Mat3u;
typedef Mat3<s32>  Mat3s;

typedef Mat4<r32>  Mat4f;
typedef Mat4<r64>  Mat4d;
typedef Mat4<real> Mat4r;
typedef Mat4<u32>  Mat4u;
typedef Mat4<s32>  Mat4s;

template<u32 T_Rows, u32 T_Cols, typename T>
bool operator==(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	bool equal = true;
	for(int i = 0; i < T_Rows*T_Cols; ++i){
		equal |= (lhs.elements[i] == rhs.elements[i]);
	}
	return equal;
}
template<typename T>
bool operator==(xen::Matrix<3, 3, T>& lhs, xen::Matrix<3, 3, T>& rhs){
	return lhs.elements[0] == rhs.elements[0] &&
	       lhs.elements[1] == rhs.elements[1] &&
	       lhs.elements[2] == rhs.elements[2] &&
	       lhs.elements[3] == rhs.elements[3] &&
	       lhs.elements[4] == rhs.elements[4] &&
           lhs.elements[5] == rhs.elements[5] &&
	       lhs.elements[6] == rhs.elements[6] &&
	       lhs.elements[7] == rhs.elements[7] &&
	       lhs.elements[8] == rhs.elements[8] &&
           lhs.elements[9] == rhs.elements[9];
}
template<typename T>
bool operator==(xen::Matrix<4, 4, T>& lhs, xen::Matrix<4, 4, T>& rhs){
	return lhs.elements[ 0] == rhs.elements[ 0] &&
	       lhs.elements[ 1] == rhs.elements[ 1] &&
	       lhs.elements[ 2] == rhs.elements[ 2] &&
	       lhs.elements[ 3] == rhs.elements[ 3] &&
	       lhs.elements[ 4] == rhs.elements[ 4] &&
           lhs.elements[ 5] == rhs.elements[ 5] &&
	       lhs.elements[ 6] == rhs.elements[ 6] &&
	       lhs.elements[ 7] == rhs.elements[ 7] &&
	       lhs.elements[ 8] == rhs.elements[ 8] &&
	       lhs.elements[ 9] == rhs.elements[ 9] &&
	       lhs.elements[10] == rhs.elements[10] &&
	       lhs.elements[11] == rhs.elements[11] &&
	       lhs.elements[12] == rhs.elements[12] &&
           lhs.elements[13] == rhs.elements[13] &&
	       lhs.elements[14] == rhs.elements[14] &&
           lhs.elements[15] == rhs.elements[15];
}
template<u32 T_Rows, u32 T_Cols, typename T>
bool operator!=(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	return !(lhs == rhs);
}




template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T> operator+=(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	for(int i = 0; i < T_Rows * T_Cols; ++i){
		lhs.elements[i] += rhs.elements[i];
	}
	return lhs;
}
template<typename T>
xen::Matrix<3,3,T> operator+=(xen::Matrix<3, 3, T>& lhs, xen::Matrix<3, 3, T>& rhs){
	lhs.elements[0] += rhs.elements[0];
	lhs.elements[1] += rhs.elements[1];
	lhs.elements[2] += rhs.elements[2];
	lhs.elements[3] += rhs.elements[3];
	lhs.elements[4] += rhs.elements[4];
	lhs.elements[5] += rhs.elements[5];
	lhs.elements[6] += rhs.elements[6];
	lhs.elements[7] += rhs.elements[7];
	lhs.elements[8] += rhs.elements[8];
	lhs.elements[9] += rhs.elements[9];
	return lhs;
}
template<typename T>
xen::Matrix<3,3,T> operator+=(xen::Matrix<4, 4, T>& lhs, xen::Matrix<4, 4, T>& rhs){
	lhs.elements[ 0] += rhs.elements[ 0];
	lhs.elements[ 1] += rhs.elements[ 1];
	lhs.elements[ 2] += rhs.elements[ 2];
	lhs.elements[ 3] += rhs.elements[ 3];
	lhs.elements[ 4] += rhs.elements[ 4];
	lhs.elements[ 5] += rhs.elements[ 5];
	lhs.elements[ 6] += rhs.elements[ 6];
	lhs.elements[ 7] += rhs.elements[ 7];
	lhs.elements[ 8] += rhs.elements[ 8];
	lhs.elements[ 9] += rhs.elements[ 9];
	lhs.elements[10] += rhs.elements[10];
	lhs.elements[11] += rhs.elements[11];
	lhs.elements[12] += rhs.elements[12];
	lhs.elements[13] += rhs.elements[13];
	lhs.elements[14] += rhs.elements[14];
	lhs.elements[15] += rhs.elements[15];
	return lhs;
}
template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T> operator+(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	xen::Matrix<T_Rows, T_Cols, T> result = lhs;
	lhs += rhs;
	return lhs;
}




template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T> operator-=(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	for(int i = 0; i < T_Rows * T_Cols; ++i){
		lhs.elements[i] -= rhs.elements[i];
	}
	return lhs;
}
template<typename T>
xen::Matrix<3,3,T> operator-=(xen::Matrix<3, 3, T>& lhs, xen::Matrix<3, 3, T>& rhs){
	lhs.elements[0] -= rhs.elements[0];
	lhs.elements[1] -= rhs.elements[1];
	lhs.elements[2] -= rhs.elements[2];
	lhs.elements[3] -= rhs.elements[3];
	lhs.elements[4] -= rhs.elements[4];
	lhs.elements[5] -= rhs.elements[5];
	lhs.elements[6] -= rhs.elements[6];
	lhs.elements[7] -= rhs.elements[7];
	lhs.elements[8] -= rhs.elements[8];
	lhs.elements[9] -= rhs.elements[9];
	return lhs;
}
template<typename T>
xen::Matrix<3,3,T> operator-=(xen::Matrix<4, 4, T>& lhs, xen::Matrix<4, 4, T>& rhs){
	lhs.elements[ 0] -= rhs.elements[ 0];
	lhs.elements[ 1] -= rhs.elements[ 1];
	lhs.elements[ 2] -= rhs.elements[ 2];
	lhs.elements[ 3] -= rhs.elements[ 3];
	lhs.elements[ 4] -= rhs.elements[ 4];
	lhs.elements[ 5] -= rhs.elements[ 5];
	lhs.elements[ 6] -= rhs.elements[ 6];
	lhs.elements[ 7] -= rhs.elements[ 7];
	lhs.elements[ 8] -= rhs.elements[ 8];
	lhs.elements[ 9] -= rhs.elements[ 9];
	lhs.elements[10] -= rhs.elements[10];
	lhs.elements[11] -= rhs.elements[11];
	lhs.elements[12] -= rhs.elements[12];
	lhs.elements[13] -= rhs.elements[13];
	lhs.elements[14] -= rhs.elements[14];
	lhs.elements[15] -= rhs.elements[15];
	return lhs;
}
template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T> operator-(xen::Matrix<T_Rows, T_Cols, T>& lhs, xen::Matrix<T_Rows, T_Cols, T>& rhs){
	xen::Matrix<T_Rows, T_Cols, T> result = lhs;
	lhs -= rhs;
	return lhs;
}
#endif
