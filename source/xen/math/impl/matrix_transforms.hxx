////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for generating transformations represented as matricies
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_IMPL_MATRIX_TRANSFORMS_HPP
#define XEN_MATH_IMPL_MATRIX_TRANSFORMS_HPP

#include <xen/math/angle.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/vector_types.hpp>

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
		real c = xen::cos(-a);
		real s = xen::sin(-a);
		return { 1,0,0,0,  0,c,-s,0,  0,s,c,0,  0,0,0,1 };
	}
	inline Mat4r Rotation3dy(Angle a){
		real c = xen::cos(-a);
		real s = xen::sin(-a);
		return { c,0,s,0,  0,1,0,0,  -s,0,c,0,  0,0,0,1 };
	}
	inline Mat4r Rotation3dz(Angle a){
		real c = xen::cos(-a);
		real s = xen::sin(-a);
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

	inline Mat4r createOrthographicProjection(real left, real right, real bottom, real top, real zNear, real zFar){
		return {                 2/(right-left),                            0,                          0,  0,
		                                      0,               2/(top-bottom),                          0,  0,
		                                      0,                            0,            -2/(zFar-zNear),  0,
               -((right+left)/(right-left)), -((top+bottom)/(top-bottom)),-((zFar+zNear)/(zFar-zNear)), 1};
	}

}

#endif
