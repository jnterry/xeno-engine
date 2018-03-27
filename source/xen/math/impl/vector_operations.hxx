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
#include "swizzles.hxx"

namespace xen{
	template<u32 T_SIZE, typename T>
	bool isnan(Vec<T_SIZE, T> vec){
		for(u32 i = 0; i < T_SIZE; ++i){
			if(xen::isnan(vec.elements[i])){ return true; }
		}
		return false;
	}

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

	/////////////////////////////////////////////////////////////////////
	/// \brief Finds element-wise maximum of two vectors
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec<2, T> max(const Vec<2, T>& a, const Vec<2, T>& b){
		return { xen::max(a.x, b.x),
				     xen::max(a.y, b.y)
				   };
	}
	template<typename T>
	Vec<3, T> max(const Vec<3, T>& a, const Vec<3, T>& b){
		return { xen::max(a.x, b.x),
				     xen::max(a.y, b.y),
				     xen::max(a.z, b.z)
				   };
	}
	template<typename T>
	Vec<4, T> max(const Vec<4, T>& a, const Vec<4, T>& b){
		return { xen::max(a.x, b.x),
				     xen::max(a.y, b.y),
				     xen::max(a.z, b.z),
				     xen::max(a.w, b.w)
				   };
	}
	template<u32 T_DIM, typename T>
	Vec<T_DIM, T> max(const Vec<T_DIM, T>& a, const Vec<T_DIM, T>& b){
		Vec<T_DIM, T> result;
		for(u32 i = 0; i < T_DIM; ++i){
			result[i] = xen::max(a[i], b[i]);
		}
		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Finds element-wise minimum of two vectors
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec<2, T> min(const Vec<2, T>& a, const Vec<2, T>& b){
		return { xen::min(a.x, b.x),
				     xen::min(a.y, b.y)
				   };
	}
	template<typename T>
	Vec<3, T> min(const Vec<3, T>& a, const Vec<3, T>& b){
		return { xen::min(a.x, b.x),
				     xen::min(a.y, b.y),
				     xen::min(a.z, b.z)
				   };
	}
	template<typename T>
	Vec<4, T> min(const Vec<4, T>& a, const Vec<4, T>& b){
		return { xen::min(a.x, b.x),
				     xen::min(a.y, b.y),
				     xen::min(a.z, b.z),
				     xen::min(a.w, b.w)
				   };
	}
	template<u32 T_DIM, typename T>
	Vec<T_DIM, T> min(const Vec<T_DIM, T>& a, const Vec<T_DIM, T>& b){
		Vec<T_DIM, T> result;
		for(u32 i = 0; i < T_DIM; ++i){
			result[i] = xen::min(a[i], b[i]);
		}
		return result;
	}

	//////////////////////////////////////////////////////////////////////////////
	// Swizzles

	// Any dimension to 2d
	template<char T_S1, char T_S2, u32 T_DIM, typename T>
	Vec<2, T> swizzle(const Vec<T_DIM, T>& v){
		static_assert(xen::impl::SwizzlePlace<T_S1>::INDEX < T_DIM,
		              "Invalid swizzle place T_S1"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S2>::INDEX < T_DIM,
		              "Invalid swizzle place T_S2"
		             );

		return {
			v[xen::impl::SwizzlePlace<T_S1>::INDEX],
			v[xen::impl::SwizzlePlace<T_S2>::INDEX],
		};
	}


	// Any dimension to 3d
  template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
	Vec<3, T> swizzle(const Vec<T_DIM, T>& v){
		static_assert(xen::impl::SwizzlePlace<T_S1>::INDEX < T_DIM,
		              "Invalid swizzle place T_S1"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S2>::INDEX < T_DIM,
		              "Invalid swizzle place T_S2"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S3>::INDEX < T_DIM,
		              "Invalid swizzle place T_S3"
		             );

		return {
			v[xen::impl::SwizzlePlace<T_S1>::INDEX],
			v[xen::impl::SwizzlePlace<T_S2>::INDEX],
			v[xen::impl::SwizzlePlace<T_S3>::INDEX],
		};
	}

	// Any dimension to 4d
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
	Vec<4, T> swizzle(const Vec<T_DIM, T>& v){
		static_assert(xen::impl::SwizzlePlace<T_S1>::INDEX < T_DIM,
		              "Invalid swizzle place T_S1"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S2>::INDEX < T_DIM,
		              "Invalid swizzle place T_S2"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S3>::INDEX < T_DIM,
		              "Invalid swizzle place T_S3"
		             );
		static_assert(xen::impl::SwizzlePlace<T_S4>::INDEX < T_DIM,
		              "Invalid swizzle place T_S4"
		             );

		return {
			v[xen::impl::SwizzlePlace<T_S1>::INDEX],
			v[xen::impl::SwizzlePlace<T_S2>::INDEX],
			v[xen::impl::SwizzlePlace<T_S3>::INDEX],
			v[xen::impl::SwizzlePlace<T_S4>::INDEX],
		};
	}
}

#endif
