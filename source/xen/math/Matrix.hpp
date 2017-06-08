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
#include <xen/math/Angle.hpp>

// gcc doesn't like the anonomous structures inside unions, disable the warning temporarily...
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

namespace xen{
	template<u32 T_Rows, u32 T_Cols, typename T>
	struct Matrix{
		Matrix(){}
		//union{
		T elements[T_Rows * T_Cols];
		//Vec<T_Cols, T> rows[T_Rows]; //:TODO: not sure if in row major or column major format yet...
		//};
	};

	template<typename T>
	struct Matrix<3,3,T>{
		//union{
			T elements[9];
		//	Vec<3, T> rows[3]; //:TODO: not sure if in row major or column major format yet...
		//};
		static const Matrix<3,3,T> Identity;
	};
	template<typename T>
	Matrix<3,3,T> const Matrix<3,3,T>::Identity = {1,0,0,   0,1,0,  0,0,1};

	template<typename T>
	struct Matrix<4,4,T>{
		//union{
			T elements[16];
		//	Vec<4, T> rows[4]; //:TODO: not sure if in row major or column major format yet...
		//};
		static const Matrix<4,4,T> Identity;
	};
	template<typename T>
	Matrix<4,4,T> const Matrix<4,4,T>::Identity = {1,0,0,0,   0,1,0,0,  0,0,1,0,  0,0,0,1};
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

namespace xen{
    inline Mat3r Rotation2d(Angle a){
		real s = xen::sin(a);
		real c = xen::cos(a);
		return {c,-s,0,  s,c,0,  0,0,1};
	}
	inline Mat3r Translation2d(real x, real y){ return { 1,0,0,  0,1,0,  x,y,1 };     }
	inline Mat3r Translation2d(Vec2r v       ){ return Translation2d(v.x, v.y);       }
	inline Mat3r Scale2d      (real x, real y){ return {x,0,0,   0,y,0,   0,0,1};     }
	inline Mat3r Scale2d      (Vec2r factors ){ return Scale2d(factors.x, factors.y); }
	inline Mat3r Scale2d      (real factor   ){ return Scale2d(factor,    factor   ); }

	inline Mat4r Rotation3dx(Angle a){
		real c = xen::cos(a);
		real s = xen::sin(a);
		return { 1,0,0,0,  0,c,-s,0,  0,s,c,0,  0,0,0,1 };
	}
	inline Mat4r Rotation3dy(Angle a){
		real c = xen::cos(a);
		real s = xen::sin(a);
		return { c,0,s,0,  0,1,0,0,  -s,0,c,0,  0,0,0,1 };
	}
	inline Mat4r Rotation3dz(Angle a){
		real c = xen::cos(a);
		real s = xen::sin(a);
		return { c,-s,0,0,  s,c,0,0,  0,0,1,0,  0,0,0,1 };
	}
	inline Mat4r Translation3d(real x, real y, real z){ return { 1,0,0,0,  0,1,0,0,  0,0,1,0,  x,y,z,1}; }
	inline Mat4r Translation3d(Vec3r v               ){ return Translation3d(v.x, v.y, v.z);             }
	inline Mat4r Scale3d      (real x, real y, real z){ return { x,0,0,0,  0,y,0,0,  0,0,z,0,  0,0,0,1}; }
	inline Mat4r Scale3d      (Vec3r factors         ){ return Scale3d(factors.x, factors.y, factors.z); }
	inline Mat4r Scale3d      (real  factors         ){ return Scale3d(factors, factors, factors);       }

	inline Mat4r createPerspectiveProjection(Angle fov_y, real width, real height, real z_near, real  z_far){
		real aspectRatio = width/height;
		real f = 1/xen::tan(fov_y / 2.0_r);

		return {  f/aspectRatio,  0,  0                                ,  0
			   ,  0            ,  f,  0                                ,  0
			   ,  0            ,  0,    (z_far+z_near) / (z_near-z_far), -1
			   ,  0            ,  0,  (2*z_far*z_near) / (z_near-z_far),  0
			   };
	}

	inline Mat4r createOrthograhpicProjection(real left, real right, real bottom, real top, real zNear, real zFar){
		return {                 2/(right-left),                            0,                          0,  0,
		                                      0,               2/(top-bottom),                          0,  0,
		                                      0,                            0,            -2/(zFar-zNear),  0,
                   -((right+left)/(right-left)), -((top+bottom)/(top-bottom)),-((zFar+zNear)/(zFar-zNear)), 1};
	}

	template<typename T>
	inline Mat4<T> transposed(const Mat4<T>& mat){
		const T* e = mat.elements;
		return { e[ 0], e[ 4], e[ 8], e[12]
			   , e[ 1], e[ 5], e[ 9], e[13]
			   , e[ 2], e[ 6], e[10], e[14]
			   , e[ 3], e[ 7], e[11], e[15]
			   };
	}

	template<typename T>
	inline T determinant(const Mat4<T>& mat){
		const T* e = mat.elements;
		return
	     e[3] * e[6] * e[ 9] * e[12] - e[2] * e[7] * e[ 9] * e[12] -
	     e[3] * e[5] * e[10] * e[12] + e[1] * e[7] * e[10] * e[12] +
	     e[2] * e[5] * e[11] * e[12] - e[1] * e[6] * e[11] * e[12] -
	     e[3] * e[6] * e[ 8] * e[13] + e[2] * e[7] * e[ 8] * e[13] +
	     e[3] * e[4] * e[10] * e[13] - e[0] * e[7] * e[10] * e[13] -
	     e[2] * e[4] * e[11] * e[13] + e[0] * e[6] * e[11] * e[13] +
	     e[3] * e[5] * e[ 8] * e[14] - e[1] * e[7] * e[ 8] * e[14] -
	     e[3] * e[4] * e[ 9] * e[14] + e[0] * e[7] * e[ 9] * e[14] +
	     e[1] * e[4] * e[11] * e[14] - e[0] * e[5] * e[11] * e[14] -
	     e[2] * e[5] * e[ 8] * e[15] + e[1] * e[6] * e[ 8] * e[15] +
	     e[2] * e[4] * e[ 9] * e[15] - e[0] * e[6] * e[ 9] * e[15] -
	     e[1] * e[4] * e[10] * e[15] + e[0] * e[5] * e[10] * e[15];

	}
}

template<u32 T_Rows, u32 T_Cols, typename T>
bool operator==(const xen::Matrix<T_Rows, T_Cols, T>& lhs, const xen::Matrix<T_Rows, T_Cols, T>& rhs){
	bool equal = true;
	for(int i = 0; i < T_Rows*T_Cols; ++i){
		equal |= (lhs.elements[i] == rhs.elements[i]);
	}
	return equal;
}
template<typename T>
bool operator==(const xen::Matrix<3, 3, T>& lhs, const xen::Matrix<3, 3, T>& rhs){
	return lhs.elements[0] - rhs.elements[0] &&
	       lhs.elements[1] - rhs.elements[1] &&
	       lhs.elements[2] - rhs.elements[2] &&
	       lhs.elements[3] - rhs.elements[3] &&
	       lhs.elements[4] - rhs.elements[4] &&
           lhs.elements[5] - rhs.elements[5] &&
	       lhs.elements[6] - rhs.elements[6] &&
	       lhs.elements[7] - rhs.elements[7] &&
	       lhs.elements[8] - rhs.elements[8] &&
           lhs.elements[9] - rhs.elements[9];
}
template<typename T>
bool operator==(const xen::Matrix<4, 4, T>& lhs, const xen::Matrix<4, 4, T>& rhs){
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
	const T* e = rhs.elements;
	//Assume w is 1, then renormalize w to 1 at the end
	// :TODO: -> test if we need to calc w and then /w at end, maybe w always comes out as 1?
	T w = lhs.x*e[ 3] + lhs.y*e[ 7] + lhs.z*e[11] + e[15];
	return { (lhs.x*e[ 0] + lhs.y*e[ 4] + lhs.z*e[ 8] + e[12]) / w
		   , (lhs.x*e[ 1] + lhs.y*e[ 5] + lhs.z*e[ 9] + e[13]) / w
		   , (lhs.x*e[ 2] + lhs.y*e[ 6] + lhs.z*e[10] + e[14]) / w
		   };
}

template<u32 T_Rows, u32 T_Cols, typename T>
xen::Matrix<T_Rows, T_Cols, T>& operator*=(xen::Matrix<T_Rows, T_Cols, T>& lhs, const xen::Matrix<T_Rows, T_Cols, T>& rhs){
	xen::Matrix<T_Rows, T_Cols, T> temp = lhs * rhs;
	lhs = temp;
	return lhs;
}

#endif
