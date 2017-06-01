////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Vector.hpp
/// \author Jamie Terry
/// \date 2017/05/30
/// \brief Contains types for representing vectors (in the mathematical sense)
/// as well as the functions to manipulate them
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_VECTOR_HPP
#define XEN_MATH_VECTOR_HPP

#include <xen/core/intrinsics.hpp>
#include "Angle.hpp"

// gcc doesn't like the anonomous structures inside unions, disable the warning temporarily...
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

namespace xen{

	template<u32 T_DIM, typename T>
	struct Vec{
		T elements[T_DIM];
	};

	template<typename T>
	struct Vec<2,T>{
		union{
		    T elements[2];
			struct{ T x, y; };
			struct{ T u, v; };
		};
		static const Vec<2, T> UnitX, UnitY, Origin;
	};
	template<typename T> const Vec<2, T> Vec<2, T>::UnitX  = {1,0};
	template<typename T> const Vec<2, T> Vec<2, T>::UnitY  = {0,1};
	template<typename T> const Vec<2, T> Vec<2, T>::Origin = {0,0};

	template<typename T>
	struct Vec<3,T>{
		union{
			T elements[3];
			struct{ T x, y, z;                     };
			struct{ T u, v;                        };
			struct{ Vec<2, T> uv;                  };
			struct{ Vec<2, T> xy;                  };
			struct{ T _unused1; Vec<2, T> yz; };
		};
		static const Vec<3, T> UnitX, UnitY, UnitZ, Origin;
	};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitX  = {1,0,0};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitY  = {0,1,0};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitZ  = {0,0,1};
	template<typename T> const Vec<3, T> Vec<3, T>::Origin = {0,0,0};

	template<typename T>
	struct Vec<4,T>{
		union{
		    T elements[4];
			struct{ T x, y, z, w;                   };
			struct{ T u, v;                         };
			struct{ Vec<2, T> xy, zw;               };
			struct{ Vec<2, T> uv;                   };
			struct{ Vec<3, T> xyz;                  };
			struct{ T _unused1; Vec<3, T> yzw; };
			struct{ T _unused2; Vec<2, T> yz;  };
		};
		static const Vec<4, T> UnitX, UnitY, UnitZ, UnitW, Origin;
	};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitX  = {1,0,0,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitY  = {0,1,0,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitZ  = {0,0,1,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitW  = {0,0,0,1};
	template<typename T> const Vec<4, T> Vec<4, T>::Origin = {0,0,0,0};
}

#pragma GCC diagnostic pop // re-enable -Wpedantic

template<typename T> using Vec2 = xen::Vec<2,T>;
template<typename T> using Vec3 = xen::Vec<3,T>;
template<typename T> using Vec4 = xen::Vec<4,T>;
typedef Vec2<u32>  Vec2u;
typedef Vec2<s32>  Vec2s;
typedef Vec2<r32>  Vec2f;
typedef Vec2<r64>  Vec2d;
typedef Vec2<real> Vec2r;
typedef Vec3<u32>  Vec3u;
typedef Vec3<s32>  Vec3s;
typedef Vec3<r32>  Vec3f;
typedef Vec3<r64>  Vec3d;
typedef Vec3<real> Vec3r;
typedef Vec4<u32>  Vec4u;
typedef Vec4<s32>  Vec4s;
typedef Vec4<r32>  Vec4f;
typedef Vec4<r64>  Vec4d;
typedef Vec4<real> Vec4r;

template<typename T>
bool operator==(const xen::Vec<2, T>& lhs, const xen::Vec<2, T>& rhs){
	return lhs.x == rhs.x && lhs.y == lhs.y;
}
template<typename T>
bool operator==(const xen::Vec<3, T>& lhs, const xen::Vec<3, T>& rhs){
	return lhs.x == rhs.x && lhs.y == lhs.y && lhs.z == rhs.z;
}
template<typename T>
bool operator==(const  xen::Vec<4, T>& lhs, const xen::Vec<4, T>& rhs){
	return lhs.x == rhs.x && lhs.y == lhs.y && lhs.z == rhs.z && lhs.z == rhs.z;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Vec<T_DIM, T>& lhs, const xen::Vec<T_DIM, T>& rhs){
	return !(lhs == rhs);
}



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
xen::Vec<3, T> operator-(const xen::Vec<3,T>& vec){
	return {-vec.x, -vec.y, -vec.z};
}

template<typename T>
xen::Vec<4, T> operator-(const xen::Vec<4,T>& vec){
	return {-vec.x, -vec.y, -vec.z, -vec.w};
}

namespace xen{
	template<typename T>
	real distanceBetween(const Vec2<T>& a, const Vec2<T>& b){
		Vec2<T> d = a - b;
		return xen::sqrt(d.x*d.x + d.y*d.y);
	}

	template<typename T>
	real distanceBetween(const Vec3<T>& a, const Vec3<T>& b){
		Vec3<T> d = a - b;
		return xen::sqrt(d.x*d.x + d.y*d.y + d.z*d.z);
	}

	template<typename T>
	real distanceBetween(const Vec4<T>& a, const Vec4<T>& b){
		Vec4<T> d = a - b;
		return xen::sqrt(d.x*d.x + d.y*d.y + d.z*d.z + d.w*d.w);
	}

	template<u32 T_Dims, typename T>
	real length(const Vec<T_Dims, T>& v){
		return distanceBetween(v, Vec<T_Dims, T>::Origin);
	}

	/// \brief Computes magnitude of vector
	template<u32 T_Dims, typename T>
	real mag(const Vec<T_Dims, T>& v){ return length(v); }

	template<u32 T_Dims, typename T>
	Vec<T_Dims, T> normalized(const Vec<T_Dims, T>& v){ return v / length(v); }

	/// \brief Computes cross product of two vectors
	template<typename T>
	Vec3<T> cross(const Vec3<T>& a, const Vec3<T>& b){
		return { a.y*b.z - a.z*b.y,  a.z*b.x - b.x*a.z, a.x*b.y - b.y*a.x};
	}

	/// \brief Computes dot product of two vectors
	template<typename T>
	T dot(const Vec3<T>& a, const Vec3<T>& b){
		return a.x*b.x + a.y*b.y + a.z*b.z;
	}

	/// \brief Computes minimum angle between two vectors - direction of angle could be
	/// either clockwise or anti-clockwise
	template<u32 T_Dims, typename T>
	inline Angle minAngleBetween(const Vec<T_Dims, T>& a, const Vec<T_Dims, T>& b){
	    return xen::acos(dot(a,b) / mag(a) * mag(b));
	}

	/// \brief Computes the clockwise angle between two vectors
	/// \todo :TODO: test -> this might be counterclockwise...
	template<typename T>
	inline Angle clockwiseAngleBetween(const Vec<3, T>& a, const Vec<3,T>& b){
		Angle min = minAngleBetween(a,b);

		Angle dir = atan2(a.x*b.y - a.y*b.x, a.x*b.x+a.y*b.y);

		if(dir > 0_deg){
			return 180_deg - min;
		} else {
			return 180_deg + min;
		}
	}

}

#endif
