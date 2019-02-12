////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for manipulating and using a viewing frustrum
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_FRUSTRUM_HPP
#define XEN_MATH_FRUSTRUM_HPP

#include "frustrum_types.hpp"
#include "matrix_types.hpp"
#include "geometry_types.hpp"
#include "plane.hpp"

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a view frustrum in world space from a view matrix
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Frustrum<T> getViewFrustrum(Mat4<T> m){
		// adapted from:
		// https://www.braynzarsoft.net/viewtutorial/q16390-34-aabb-cpu-side-frustum-culling
		Frustrum<T> f;

		// Left Frustum Plane
		f.plane[Frustrum<T>::Left].normal.x = m[0][3] + m[0][0];
		f.plane[Frustrum<T>::Left].normal.y = m[1][3] + m[1][0];
		f.plane[Frustrum<T>::Left].normal.z = m[2][3] + m[2][0];
		f.plane[Frustrum<T>::Left].d        = m[3][3] + m[3][0];

    // Right Frustum Plane
		f.plane[Frustrum<T>::Right].normal.x = m[0][3] - m[0][0];
		f.plane[Frustrum<T>::Right].normal.y = m[1][3] - m[1][0];
		f.plane[Frustrum<T>::Right].normal.z = m[2][3] - m[2][0];
		f.plane[Frustrum<T>::Right].d        = m[3][3] - m[3][0];

    // Top Frustum Plane
    f.plane[Frustrum<T>::Top].normal.x = m[0][3] - m[0][1];
    f.plane[Frustrum<T>::Top].normal.y = m[1][3] - m[1][1];
    f.plane[Frustrum<T>::Top].normal.z = m[2][3] - m[2][1];
    f.plane[Frustrum<T>::Top].d        = m[3][3] - m[3][1];

    // Bottom Frustum Plane
    f.plane[Frustrum<T>::Bottom].normal.x = m[0][3] + m[0][1];
    f.plane[Frustrum<T>::Bottom].normal.y = m[1][3] + m[1][1];
    f.plane[Frustrum<T>::Bottom].normal.z = m[2][3] + m[2][1];
    f.plane[Frustrum<T>::Bottom].d        = m[3][3] + m[3][1];

    // Near Frustum Plane
    f.plane[Frustrum<T>::Near].normal.x = m[0][2];
    f.plane[Frustrum<T>::Near].normal.y = m[1][2];
    f.plane[Frustrum<T>::Near].normal.z = m[2][2];
    f.plane[Frustrum<T>::Near].d        = m[3][2];

    // Far Frustum Plane
    f.plane[Frustrum<T>::Far].normal.x = m[0][3] - m[0][2];
    f.plane[Frustrum<T>::Far].normal.y = m[1][3] - m[1][2];
    f.plane[Frustrum<T>::Far].normal.z = m[2][3] - m[2][2];
    f.plane[Frustrum<T>::Far].d        = m[3][3] - m[3][2];

    // Normalize all
    for(int i = 0; i < 6; ++i){
	    //f.plane[i].normal = xen::normalized(f.plane[i].normal);
	    ((Vec4r&)f.plane[i]) /= xen::length((Vec4r&)f.plane[i]);
    }

    return f;
	}

	template<typename T>
	bool hasIntersection(Frustrum<T> f, Aabb3<T> box){
		// using p n vertex optimization, rathe than testing all points of Aabb
		// against each plane we instead check the vertex furthest along the normal
		// of the plane
		// https://www.gamedev.net/forums/topic/512123-fast--and-correct-frustum---aabb-intersection/
		// https://fgiesen.wordpress.com/2010/10/17/view-frustum-culling/
		Vec3r v;
		for(int i = 0; i < 6; ++i){
			for(int ax = 0; ax < 3; ++ax){
				if(f.plane[i].normal[ax] < 0){
					v[ax] = box.min[ax];
				} else {
					v[ax] = box.max[ax];
				}
			}

			if(xen::getSignedDistanceToPlane(f.plane[i], v) < 0){
				return false;
			}
		}
		return true;
	}

	template<typename T>
	Frustrum<T> getTransformed(Frustrum<T> f, Mat4<T> mat){
		Frustrum<T> result;
		for(int i = 0; i < 6; ++i){
			result.plane[i] = xen::getTransformed(f.plane[i], mat);
		}
		return result;
	}
}

#endif
