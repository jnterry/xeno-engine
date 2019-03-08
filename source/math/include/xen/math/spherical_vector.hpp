////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for vectors upon the surface of the unit sphere
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_SPHERICAL_VECTOR_HPP
#define XEN_MATH_SPHERICAL_VECTOR_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/quaternion.hpp>
#include <xen/math/vector.hpp>

namespace xen {

	typedef Vec2<xen::Angle> LatLong;

	// See: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.217.7801&rep=rep1&type=pdf
	template<typename T>
	struct SphericalVector {
		T swivel;
		T distance;
	};

	Vec3r toCartesian(const LatLong& v, real radius = 1){
		real cy = xen::cos(v.y);
		real sy = xen::sin(v.y);
		return radius * Vec3r{
			cy * xen::sin(v.x),
			xen::sin(v.y),
			cy * xen::cos(v.x),
		};
	}
  LatLong toLatLong(Vec3r v){
		v = xen::normalized(v);
		LatLong result;
		result.y = xen::asin(v.y);
		real cy  = xen::cos(result.y);
		result.x = xen::asin(v.x / cy);
		return result;
	}

	//xen::Angle getAngleSubstended(const SphericalVector<T>& p, const SphericalVector<T>& q){
	//
	//}

	template<typename T>
	Vec3<T> toCartesian(const SphericalVector<T>& v, T radius = 1){
		/*
		Vec3<T> result = Vec3r::UnitZ * radius;
		result = xen::rotated(result, Vec3r::UnitY, xen::Revolutions(v.x));
		result = xen::rotated(result,
		                      xen::rotated(Vec3r::UnitX, Vec3r::UnitY, xen::Revolutions(v.x)),
		                      xen::Revolutions(v.y));

		printf("Returning %f, %f, %f\n", result.x, result.y, result.z);
		return result;*/

		/*

		Vec3<T> result = Vec3r{1, 1, 0 };
		Vec3<T> axis = xen::rotated(Vec3r::UnitY, Vec3r::UnitX, xen::Revolutions(v.inclination));
		result = xen::rotated(result, axis, xen::Revolutions(v.epoch));
		return result;

		*/
	}
}

template<typename T>
xen::LatLong operator+(xen::LatLong& a, xen::LatLong& b){
	return { a.x + b.x, a.y + b.y };
}
template<typename T>
xen::LatLong operator-(const xen::LatLong& a, const xen::LatLong& b){
	return { a.x - b.x, a.y - b.y };
}

/*

template<typename T>
xen::LatLong operator+(const xen::LatLong& l, const xen::SphericalVector<T>& v){
}

template<typename T>
xen::SphericalVector<T> operator-(const xen::SphericalVector<T>& p, const xen::SphericalVector<T>& q){

}
*/


#endif
