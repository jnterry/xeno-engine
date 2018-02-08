////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \file matrix_transforms.hxx
///
/// \brief Contains functions for performing mathematical operations on matrices
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_IMPL_MATRIX_OPERATIONS_HPP
#define XEN_MATH_IMPL_MATRIX_OPERATIONS_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/angle.hpp>

namespace xen {
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

	// Returns the inverse of the matrix given
	// BEWARE: Here lies poorly documented spaghetti code
	// It shouldn't need altering unless I've missed some null/swap cases
	template <typename T>
	Mat3<T> getInverse(const Mat3<T>& mat) {
		Mat3<T> become_ident = mat; // Matrix that becomes identity as other becomes inverse
		Mat3<T> become_invrs  = Mat3<T>::Identity; // Matrix that becomes the inverse via Gauss-Jordann

	  T ratio;
		// Zero first element of second row
		ratio = mat.elements[1] / mat.elements[0];
		become_ident.elements[1] -= ratio * become_ident.elements[0];
		become_ident.elements[4] -= ratio * become_ident.elements[3];
		become_ident.elements[7] -= ratio * become_ident.elements[6];
		become_invrs.elements[1] -= ratio * become_invrs.elements[0];
		become_invrs.elements[4] -= ratio * become_invrs.elements[3];
		become_invrs.elements[7] -= ratio * become_invrs.elements[6];

		// Zero first element of third row
		ratio = mat.elements[2] / mat.elements[0];
		become_ident.elements[2] -= ratio * become_ident.elements[0];
		become_ident.elements[5] -= ratio * become_ident.elements[3];
		become_ident.elements[8] -= ratio * become_ident.elements[6];
		become_invrs.elements[2] -= ratio * become_invrs.elements[0];
		become_invrs.elements[5] -= ratio * become_invrs.elements[3];
		become_invrs.elements[8] -= ratio * become_invrs.elements[6];

		// Check rows do not zero and do not require swap
		if ((become_ident.elements[4] == 0) && (become_ident.elements[7] == 0)) {
			return Mat3<T>::Zero;
		} else if ((become_ident.elements[4] == 0)) {
			if (become_ident.elements[5] == 0) {
				return Mat3<T>::Zero;
			} else {
				become_ident.swapRow(1,2);
			}
		}

		// Zero second element of third row
		ratio = become_ident.elements[5] / become_ident.elements[4];
		become_ident.elements[5] -= ratio * become_ident.elements[4];
		become_ident.elements[8] -= ratio * become_ident.elements[7];
		become_invrs.elements[2] -= ratio * become_invrs.elements[1];
		become_invrs.elements[5] -= ratio * become_invrs.elements[4];
		become_invrs.elements[8] -= ratio * become_invrs.elements[7];

		// Check rows do not zero and do not require swap
		if (become_ident.elements[8] == 0) {
			return Mat3<T>::Zero;
		}

		// Zero out last column of first row
		ratio = become_ident.elements[6] / become_ident.elements[8];
		become_ident.elements[6] -= ratio * become_ident.elements[8];
		become_invrs.elements[0] -= ratio * become_invrs.elements[2];
		become_invrs.elements[3] -= ratio * become_invrs.elements[5];
		become_invrs.elements[6] -= ratio * become_invrs.elements[8];

		// Zero out last column of second row
		ratio = become_ident.elements[7] / become_ident.elements[8];
		become_ident.elements[7] -= ratio * become_ident.elements[8];
		become_invrs.elements[1] -= ratio * become_invrs.elements[2];
		become_invrs.elements[4] -= ratio * become_invrs.elements[5];
		become_invrs.elements[7] -= ratio * become_invrs.elements[8];

		// Zero out middle column of first row
		ratio = become_ident.elements[3] / become_ident.elements[4];
		become_ident.elements[3] -= ratio * become_ident.elements[4];
		become_invrs.elements[0] -= ratio * become_invrs.elements[1];
		become_invrs.elements[3] -= ratio * become_invrs.elements[4];
		become_invrs.elements[6] -= ratio * become_invrs.elements[7];

		// Make becomeIdentity the identity
		// First row
		ratio = become_ident.elements[0];
		become_ident.elements[0] /= ratio;
		become_invrs.elements[0] /= ratio;
		become_invrs.elements[3] /= ratio;
		become_invrs.elements[6] /= ratio;
		// Second row
		ratio = become_ident.elements[4];
		become_ident.elements[4] /= ratio;
		become_invrs.elements[1] /= ratio;
		become_invrs.elements[4] /= ratio;
		become_invrs.elements[7] /= ratio;
		// Third row
		ratio = become_ident.elements[8];
		become_ident.elements[8] /= ratio;
		become_invrs.elements[2] /= ratio;
		become_invrs.elements[5] /= ratio;
		become_invrs.elements[8] /= ratio;

		return become_invrs;
	}

	template <typename T>
	Mat4<T> getInverse(const Mat4<T>& mat) {
		// Adapted from MESA's glu implementation
		Mat4<T> inverse;

		T det;

		const T* m  = mat.elements;
		T* inv      = inverse.elements;

    inv[0] = m[5]  * m[10] * m[15] -
	    m[5]  * m[11] * m[14] -
	    m[9]  * m[6]  * m[15] +
	    m[9]  * m[7]  * m[14] +
	    m[13] * m[6]  * m[11] -
	    m[13] * m[7]  * m[10];

    inv[4] = -m[4]  * m[10] * m[15] +
	    m[4]  * m[11] * m[14] +
	    m[8]  * m[6]  * m[15] -
	    m[8]  * m[7]  * m[14] -
	    m[12] * m[6]  * m[11] +
	    m[12] * m[7]  * m[10];

    inv[8] = m[4]  * m[9] * m[15] -
	    m[4]  * m[11] * m[13] -
	    m[8]  * m[5] * m[15] +
	    m[8]  * m[7] * m[13] +
	    m[12] * m[5] * m[11] -
	    m[12] * m[7] * m[9];

    inv[12] = -m[4]  * m[9] * m[14] +
	    m[4]  * m[10] * m[13] +
	    m[8]  * m[5] * m[14] -
	    m[8]  * m[6] * m[13] -
	    m[12] * m[5] * m[10] +
	    m[12] * m[6] * m[9];

    inv[1] = -m[1]  * m[10] * m[15] +
	    m[1]  * m[11] * m[14] +
	    m[9]  * m[2] * m[15] -
	    m[9]  * m[3] * m[14] -
	    m[13] * m[2] * m[11] +
	    m[13] * m[3] * m[10];

    inv[5] = m[0]  * m[10] * m[15] -
	    m[0]  * m[11] * m[14] -
	    m[8]  * m[2] * m[15] +
	    m[8]  * m[3] * m[14] +
	    m[12] * m[2] * m[11] -
	    m[12] * m[3] * m[10];

    inv[9] = -m[0]  * m[9] * m[15] +
	    m[0]  * m[11] * m[13] +
	    m[8]  * m[1] * m[15] -
	    m[8]  * m[3] * m[13] -
	    m[12] * m[1] * m[11] +
	    m[12] * m[3] * m[9];

    inv[13] = m[0]  * m[9] * m[14] -
	    m[0]  * m[10] * m[13] -
	    m[8]  * m[1] * m[14] +
	    m[8]  * m[2] * m[13] +
	    m[12] * m[1] * m[10] -
	    m[12] * m[2] * m[9];

    inv[2] = m[1]  * m[6] * m[15] -
	    m[1]  * m[7] * m[14] -
	    m[5]  * m[2] * m[15] +
	    m[5]  * m[3] * m[14] +
	    m[13] * m[2] * m[7] -
	    m[13] * m[3] * m[6];

    inv[6] = -m[0]  * m[6] * m[15] +
	    m[0]  * m[7] * m[14] +
	    m[4]  * m[2] * m[15] -
	    m[4]  * m[3] * m[14] -
	    m[12] * m[2] * m[7] +
	    m[12] * m[3] * m[6];

    inv[10] = m[0]  * m[5] * m[15] -
	    m[0]  * m[7] * m[13] -
	    m[4]  * m[1] * m[15] +
	    m[4]  * m[3] * m[13] +
	    m[12] * m[1] * m[7] -
	    m[12] * m[3] * m[5];

    inv[14] = -m[0]  * m[5] * m[14] +
	    m[0]  * m[6] * m[13] +
	    m[4]  * m[1] * m[14] -
	    m[4]  * m[2] * m[13] -
	    m[12] * m[1] * m[6] +
	    m[12] * m[2] * m[5];

    inv[3] = -m[1] * m[6] * m[11] +
	    m[1] * m[7] * m[10] +
	    m[5] * m[2] * m[11] -
	    m[5] * m[3] * m[10] -
	    m[9] * m[2] * m[7] +
	    m[9] * m[3] * m[6];

    inv[7] = m[0] * m[6] * m[11] -
	    m[0] * m[7] * m[10] -
	    m[4] * m[2] * m[11] +
	    m[4] * m[3] * m[10] +
	    m[8] * m[2] * m[7] -
	    m[8] * m[3] * m[6];

    inv[11] = -m[0] * m[5] * m[11] +
	    m[0] * m[7] * m[9] +
	    m[4] * m[1] * m[11] -
	    m[4] * m[3] * m[9] -
	    m[8] * m[1] * m[7] +
	    m[8] * m[3] * m[5];

    inv[15] = m[0] * m[5] * m[10] -
	    m[0] * m[6] * m[9] -
	    m[4] * m[1] * m[10] +
	    m[4] * m[2] * m[9] +
	    m[8] * m[1] * m[6] -
	    m[8] * m[2] * m[5];

    det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

    if (det == 0) {
	    return Mat4<T>::Zero;
    }

    det = 1.0 / det;

    inverse *= det;

    return inverse;
	}
}

#endif
