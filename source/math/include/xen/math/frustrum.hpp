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

		//m = xen::transposed(m);

		// Left Frustum Plane
    // Add first column of the matrix to the fourth column
    f.plane[0].normal.x = m[0][3] + m[0][0];
    f.plane[0].normal.y = m[1][3] + m[1][0];
    f.plane[0].normal.z = m[2][3] + m[2][0];
    f.plane[0].d        = m[3][3] + m[3][0];

    // Right Frustum Plane
    // Subtract first column of matrix from the fourth column
    f.plane[1].normal.x = m[0][3] - m[0][0];
    f.plane[1].normal.y = m[1][3] - m[1][0];
    f.plane[1].normal.z = m[2][3] - m[2][0];
    f.plane[1].d        = m[3][3] - m[3][0];

    // Top Frustum Plane
    // Subtract second column of matrix from the fourth column
    f.plane[2].normal.x = m[0][3] - m[0][1];
    f.plane[2].normal.y = m[1][3] - m[1][1];
    f.plane[2].normal.z = m[2][3] - m[2][1];
    f.plane[2].d        = m[3][3] - m[3][1];

    // Bottom Frustum Plane
    // Add second column of the matrix to the fourth column
    f.plane[3].normal.x = m[0][3] + m[0][1];
    f.plane[3].normal.y = m[1][3] + m[1][1];
    f.plane[3].normal.z = m[2][3] + m[2][1];
    f.plane[3].d        = m[3][3] + m[3][1];

    // Near Frustum Plane
    // We could add the third column to the fourth column to get the near plane,
    // but we don't have to do this because the third column IS the near plane
    f.plane[4].normal.x = m[0][2];
    f.plane[4].normal.y = m[1][2];
    f.plane[4].normal.z = m[2][2];
    f.plane[4].d        = m[3][2];

    // Far Frustum Plane
    // Subtract third column of matrix from the fourth column
    f.plane[5].normal.x = m[0][3] - m[0][2];
    f.plane[5].normal.y = m[1][3] - m[1][2];
    f.plane[5].normal.z = m[2][3] - m[2][2];
    f.plane[5].d        = m[3][3] - m[3][2];

    // Normalize all
    for(int i = 0; i < 6; ++i){
	    f.plane[i].normal = xen::normalized(f.plane[i].normal);
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
