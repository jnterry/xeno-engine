////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains basic arithmetic operations which operate on vectors
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_IMPL_VECTOR_OPERATIONS_HXX
#define XEN_MATH_IMPL_VECTOR_OPERATIONS_HXX

#include <xen/math/vector_types.hpp>
#include <xen/math/angle.hpp>

namespace xen{
	/// \brief Computes dot product of two vectors
	template<typename T>
	T dot(const Vec2<T>& a, const Vec2<T>& b){
		return a.x*b.x + a.y*b.y;
	}
	template<typename T>
	T dot(const Vec3<T>& a, const Vec3<T>& b){
		return a.x*b.x + a.y*b.y + a.z*b.z;
	}
	template<typename T>
	T dot(const Vec4<T>& a, const Vec4<T>& b){
		return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w;
	}

	/// \brief Computes cross product of two vectors
	template<typename T>
	Vec3<T> cross(const Vec3<T>& a, const Vec3<T>& b){
		return { a.y*b.z - a.z*b.y
			   , a.z*b.x - a.x*b.z
			   , a.x*b.y - a.y*b.x };
	}

	template<u32 T_DIM, typename T>
	real distanceSq(const Vec<T_DIM, T>& a, const Vec<T_DIM, T>& b){
		Vec<T_DIM, T> d = a - b;
		return dot(d, d);
	}

	template<u32 T_DIM, typename T>
	real distance(const Vec<T_DIM, T>& a, const Vec<T_DIM, T>& b){
		return xen::sqrt(distanceSq<T_DIM, T>(a,b));
	}

	template<u32 T_Dims, typename T>
	real lengthSq(const Vec<T_Dims, T>& v){
		return distanceSq<T_Dims, T>(v, Vec<T_Dims, T>::Origin);
	}

	template<u32 T_Dims, typename T>
	real length(const Vec<T_Dims, T>& v){
		return distance(v, Vec<T_Dims, T>::Origin);
	}

	/// \brief Computes magnitude of vector
	template<u32 T_Dims, typename T>
	real mag(const Vec<T_Dims, T>& v){ return length(v); }

	/// \brief Computes magnitude of vector squared
	template<u32 T_Dims, typename T>
	real magSq(const Vec<T_Dims, T>& v){ return lengthSq(v); }

	template<u32 T_Dims, typename T>
	Vec<T_Dims, T> normalized(const Vec<T_Dims, T>& v){ return v / length(v); }

	/// \brief Computes minimum angle between two vectors - direction of angle could be
	/// either clockwise or anti-clockwise
	template<u32 T_Dims, typename T>
	inline Angle angleBetween(const Vec<T_Dims, T>& a, const Vec<T_Dims, T>& b){
		return xen::acos(dot(a,b) / (mag(a) * mag(b)));
	}

	/// \brief Projects the direction vector vec onto the plane defined by the normal vector norm
	template<typename T>
	inline Vec3<T> projectOntoPlane(Vec3<T> vec, Vec3<T> norm){
		return vec - ((dot(vec,norm) / magSq(norm)) * norm);
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Converts from a 4d homogeneous coordinate space to a 3d
	/// coordinate space
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec3<T> fromHomo(const Vec4<T>& v){
		return v.xyz / v.w;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Converts from a 3d space to a 4d homogeneous coordinate space
	/// \param val_w The value to use for the w component, defaults to 1
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec4<T> toHomo(const Vec3<T>& v, T val_w = 1){
		return {v.x, v.y, v.z, val_w};
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Converts from a 3d homogeneous coordinate space to a 2d
	/// coordinate space
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec2<T> fromHomo(const Vec3<T>& v){
		return v.xy / v.z;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Converts from a 2d space to a 3d homogeneous coordinate space
	/// \param val_z The value to use for the z component, defaults to 1
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec3<T> toHomo(const Vec2<T>& v, T val_z = 1){
		return {v.x, v.y, val_z};
	}
}

#endif
