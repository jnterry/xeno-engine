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

#include <xen/math/Quaternion.hpp>

namespace xen{
	const Quaternion Quaternion::Identity = {0,0,0,1};

	AxisAngle::AxisAngle(Quaternion q){
		real mag = length(q.xyz);
		this->axis  = q.xyz / mag;
		this->angle = xen::atan2(mag, q.w) * 2;
	}
}
