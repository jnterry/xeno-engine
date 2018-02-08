////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file Quaternion.cpp
/// \author Jamie Terry
/// \date 2017/06/03
/// \brief Contains implementation details for xen::Quaternion
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/quaternion.hpp>
#include <xen/math/vector.hpp>

namespace xen{
	const Quaternion Quaternion::Identity = {0,0,0,1};

	Quaternion::Quaternion(Vec3r axis, Angle a){
			a *= 0.5;
			this->xyz = normalized(axis) * xen::sin(a);
			this->w   = xen::cos(a);
		}

	AxisAngle::AxisAngle(Quaternion q){
		real mag = length(q.xyz);
		this->axis  = q.xyz / mag;
		this->angle = xen::atan2(mag, q.w) * 2;
	}
}
