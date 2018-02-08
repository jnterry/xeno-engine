////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file matrix_arithmetic.hxx
///
/// \brief Contains basic arithmetic operations which operate on matrices
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_IMPL_MATRIX_ARITHEMTIC_HPP
#define XEN_MATH_IMPL_MATRIX_ARITHEMTIC_HPP

#include <xen/math/matrix_types.hpp>
#include <xen/math/Vector.hpp>

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


template<typename T>
xen::Matrix<3,3, T> operator*(const xen::Matrix<3,3, T>& lhs, const xen::Matrix<3,3, T>& rhs){
	return { lhs.elements[0]*rhs.elements[0] + lhs.elements[1]*rhs.elements[3] + lhs.elements[2]*rhs.elements[6]
			    , lhs.elements[0]*rhs.elements[1] + lhs.elements[1]*rhs.elements[4] + lhs.elements[2]*rhs.elements[7]
			    , lhs.elements[0]*rhs.elements[2] + lhs.elements[1]*rhs.elements[5] + lhs.elements[2]*rhs.elements[8]

			    , lhs.elements[3]*rhs.elements[0] + lhs.elements[4]*rhs.elements[3] + lhs.elements[5]*rhs.elements[6]
			    , lhs.elements[3]*rhs.elements[1] + lhs.elements[4]*rhs.elements[4] + lhs.elements[5]*rhs.elements[7]
			    , lhs.elements[3]*rhs.elements[2] + lhs.elements[4]*rhs.elements[5] + lhs.elements[5]*rhs.elements[8]

			    , lhs.elements[6]*rhs.elements[0] + lhs.elements[7]*rhs.elements[3] + lhs.elements[8]*rhs.elements[6]
			    , lhs.elements[6]*rhs.elements[1] + lhs.elements[7]*rhs.elements[4] + lhs.elements[8]*rhs.elements[7]
			    , lhs.elements[6]*rhs.elements[2] + lhs.elements[7]*rhs.elements[5] + lhs.elements[8]*rhs.elements[8]
			   };
}

template<typename T>
xen::Matrix<3,3, T> operator*(const xen::Matrix<3,3, T>& lhs, T rhs){
	return {
		lhs.elements[0] * rhs,
		lhs.elements[1] * rhs,
		lhs.elements[2] * rhs,
		lhs.elements[3] * rhs,
		lhs.elements[4] * rhs,
		lhs.elements[5] * rhs,
		lhs.elements[6] * rhs,
		lhs.elements[7] * rhs,
		lhs.elements[8] * rhs
	};
}

template<typename T>
xen::Matrix<4,4, T> operator*(const xen::Matrix<4,4, T>& lhs, T rhs){
	return {
		lhs.elements[ 0] * rhs,
		lhs.elements[ 1] * rhs,
		lhs.elements[ 2] * rhs,
		lhs.elements[ 3] * rhs,
		lhs.elements[ 4] * rhs,
		lhs.elements[ 5] * rhs,
		lhs.elements[ 6] * rhs,
		lhs.elements[ 7] * rhs,
		lhs.elements[ 8] * rhs,
		lhs.elements[ 9] * rhs,
		lhs.elements[10] * rhs,
		lhs.elements[11] * rhs,
		lhs.elements[12] * rhs,
		lhs.elements[13] * rhs,
		lhs.elements[14] * rhs,
		lhs.elements[15] * rhs
	};
}

template<typename T>
xen::Matrix<3,3, T>& operator*=(xen::Matrix<3,3, T>& lhs, T rhs){
	lhs.elements[0] *= rhs;
	lhs.elements[1] *= rhs;
	lhs.elements[2] *= rhs;
	lhs.elements[3] *= rhs;
	lhs.elements[4] *= rhs;
	lhs.elements[5] *= rhs;
	lhs.elements[6] *= rhs;
	lhs.elements[7] *= rhs;
	lhs.elements[8] *= rhs;
	return lhs;
}

template<typename T>
xen::Matrix<4,4, T>& operator*=(xen::Matrix<4,4, T>& lhs, T rhs){
	lhs.elements[ 0] *= rhs;
	lhs.elements[ 1] *= rhs;
	lhs.elements[ 2] *= rhs;
	lhs.elements[ 3] *= rhs;
	lhs.elements[ 4] *= rhs;
	lhs.elements[ 5] *= rhs;
	lhs.elements[ 6] *= rhs;
	lhs.elements[ 7] *= rhs;
	lhs.elements[ 8] *= rhs;
	lhs.elements[ 9] *= rhs;
	lhs.elements[10] *= rhs;
	lhs.elements[11] *= rhs;
	lhs.elements[12] *= rhs;
	lhs.elements[13] *= rhs;
	lhs.elements[14] *= rhs;
	lhs.elements[15] *= rhs;
	return lhs;
}

template<typename T>
xen::Matrix<4,4, T> operator*(const xen::Matrix<4,4,T>& lhs, const xen::Matrix<4,4,T>& rhs){
	return { lhs.elements[ 0] * rhs.elements[ 0] + lhs.elements[ 1] * rhs.elements[ 4] +
	         lhs.elements[ 2] * rhs.elements[ 8] + lhs.elements[ 3] * rhs.elements[12]

			   , lhs.elements[ 0] * rhs.elements[ 1] + lhs.elements[ 1] * rhs.elements[ 5] +
			     lhs.elements[ 2] * rhs.elements[ 9] + lhs.elements[ 3] * rhs.elements[13]

			   , lhs.elements[ 0] * rhs.elements[ 2] + lhs.elements[ 1] * rhs.elements[ 6] +
			     lhs.elements[ 2] * rhs.elements[10] + lhs.elements[ 3] * rhs.elements[14]

			   , lhs.elements[ 0] * rhs.elements[ 3] + lhs.elements[ 1] * rhs.elements[ 7] +
			     lhs.elements[ 2] * rhs.elements[11] + lhs.elements[ 3] * rhs.elements[15]


			   , lhs.elements[ 4] * rhs.elements[ 0] + lhs.elements[ 5] * rhs.elements[ 4] +
			     lhs.elements[ 6] * rhs.elements[ 8] + lhs.elements[ 7] * rhs.elements[12]

			   , lhs.elements[ 4] * rhs.elements[ 1] + lhs.elements[ 5] * rhs.elements[ 5] +
			     lhs.elements[ 6] * rhs.elements[ 9] + lhs.elements[ 7] * rhs.elements[13]

			   , lhs.elements[ 4] * rhs.elements[ 2] + lhs.elements[ 5] * rhs.elements[ 6] +
			     lhs.elements[ 6] * rhs.elements[10] + lhs.elements[ 7] * rhs.elements[14]

			   , lhs.elements[ 4] * rhs.elements[ 3] + lhs.elements[ 5] * rhs.elements[ 7] +
			     lhs.elements[ 6] * rhs.elements[11] + lhs.elements[ 7] * rhs.elements[15]


			   , lhs.elements[ 8] * rhs.elements[ 0] + lhs.elements[ 9] * rhs.elements[ 4] +
			     lhs.elements[10] * rhs.elements[ 8] + lhs.elements[11] * rhs.elements[12]

			   , lhs.elements[ 8] * rhs.elements[ 1] + lhs.elements[ 9] * rhs.elements[ 5] +
			     lhs.elements[10] * rhs.elements[ 9] + lhs.elements[11] * rhs.elements[13]

			   , lhs.elements[ 8] * rhs.elements[ 2] + lhs.elements[ 9] * rhs.elements[ 6] +
			     lhs.elements[10] * rhs.elements[10] + lhs.elements[11] * rhs.elements[14]

			   , lhs.elements[ 8] * rhs.elements[ 3] + lhs.elements[ 9] * rhs.elements[ 7] +
			     lhs.elements[10] * rhs.elements[11] + lhs.elements[11] * rhs.elements[15]



			   , lhs.elements[12] * rhs.elements[ 0] + lhs.elements[13] * rhs.elements[ 4] +
			     lhs.elements[14] * rhs.elements[ 8] + lhs.elements[15] * rhs.elements[12]

			   , lhs.elements[12] * rhs.elements[ 1] + lhs.elements[13] * rhs.elements[ 5] +
			     lhs.elements[14] * rhs.elements[ 9] + lhs.elements[15] * rhs.elements[13]

			   , lhs.elements[12] * rhs.elements[ 2] +lhs.elements[13] * rhs.elements[ 6] +
			     lhs.elements[14] * rhs.elements[10] +lhs.elements[15] * rhs.elements[14]

			   , lhs.elements[12] * rhs.elements[ 3] +lhs.elements[13] * rhs.elements[ 7] +
			     lhs.elements[14] * rhs.elements[11] +lhs.elements[15] * rhs.elements[15]
			};
}

template<typename T>
xen::Vec<4,T> operator*(const xen::Vec<4,T>& lhs, const xen::Matrix<4,4,T>& rhs){
	const T* e = rhs.elements;
	return { lhs.x*e[ 0] + lhs.y*e[ 4] + lhs.z*e[ 8] + lhs.w*e[12]
		   , lhs.x*e[ 1] + lhs.y*e[ 5] + lhs.z*e[ 9] + lhs.w*e[13]
		   , lhs.x*e[ 2] + lhs.y*e[ 6] + lhs.z*e[10] + lhs.w*e[14]
		   , lhs.x*e[ 3] + lhs.y*e[ 7] + lhs.z*e[11] + lhs.w*e[15]
		   };
}

template<typename T>
xen::Vec<3,T> operator*(const xen::Vec<3,T>& lhs, const xen::Matrix<4,4,T>& rhs){
	//To do a vec3 * mat4 we assume w is 1, and then renormalize w to 1 at the end

	const T* e = rhs.elements;
	T w =     lhs.x*e[ 3] + lhs.y*e[ 7] + lhs.z*e[11] + e[15];
	return { (lhs.x*e[ 0] + lhs.y*e[ 4] + lhs.z*e[ 8] + e[12]) / w
		     , (lhs.x*e[ 1] + lhs.y*e[ 5] + lhs.z*e[ 9] + e[13]) / w
		     , (lhs.x*e[ 2] + lhs.y*e[ 6] + lhs.z*e[10] + e[14]) / w
		     };
}

template<typename T>
xen::Vec<2,T> operator*(const xen::Vec<2,T>& lhs, const xen::Matrix<3,3,T>& rhs){
	//To do a vec2 * mat3 we assume z is 1, and then renormalize z to 1 at the end

	const T* e = rhs.elements;
	T z =     lhs.x*e[2] + lhs.y*e[5] + e[8];
	return { (lhs.x*e[0] + lhs.y*e[3] + e[6]) / z
		     , (lhs.x*e[1] + lhs.y*e[4] + e[7]) / z
		     };
}



template<typename T>
xen::Vec<3,T> operator*=(xen::Vec<3,T>& lhs, const xen::Matrix<4,4,T>& rhs){
	lhs = lhs * rhs;
	return lhs;
}

template<typename T>
xen::Vec<2,T> operator*=(xen::Vec<2,T>& lhs, const xen::Matrix<3,3,T>& rhs){
	lhs = lhs * rhs;
	return lhs;
}

template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T>& operator*=(xen::Matrix<T_Rows, T_Cols, T>& lhs, const xen::Matrix<T_Rows, T_Cols, T>& rhs){
	xen::Matrix<T_Rows, T_Cols, T> temp = lhs * rhs;
	lhs = temp;
	return lhs;
}

#endif
