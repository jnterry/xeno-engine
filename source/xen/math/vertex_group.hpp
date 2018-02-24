////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains functions for manipulating the types defined in
/// vertex_group_types.hpp
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_VERTEX_GROUP_HPP
#define XEN_VERTEX_GROUP_HPP

#include <xen/math/vertex_group_types.hpp>
#include <xen/math/matrix.hpp>
#include <xen/math/vector.hpp>
#include <xen/math/utilities.hpp>

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the barycentric coordinates for a
	/// point p with respect to a given triangle
	/// Taken from: https://gamedev.stackexchange.com/a/23745
	/// \param p The point to find barycentric coordinates of
	/// \param tri The triangle Barycentric coordinates are with respect to
	/// \return Barycentric coordinates of point p
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec3<T> getBarycentricCoordinates(const Triangle3<T>& tri, const Vec3<T>& p){
    Vec3<T> v0 = tri.p2 - tri.p1;
		Vec3<T> v1 = tri.p3 - tri.p1;
		Vec3<T> v2 = p - tri.p1;

    T d00 = xen::dot(v0, v0);
    T d01 = xen::dot(v0, v1);
    T d11 = xen::dot(v1, v1);
    T d20 = xen::dot(v2, v0);
    T d21 = xen::dot(v2, v1);
    T denom = d00 * d11 - d01 * d01;
    T v = (d11 * d20 - d01 * d21) / denom;
    T w = (d00 * d21 - d01 * d20) / denom;
    T u = 1.0f - v - w;
		return {u,v,w};
	}

	template<u32 T_DIM, typename T>
	bool hasArea(Triangle<T_DIM, T> tri){
		return tri.p1 != tri.p2 && tri.p1 != tri.p3 && tri.p2 != tri.p2;
	}

	template <typename T>
	Vec3<T> computeNormal(Triangle<3, T> tri){
		// Compute two edges
		Vec3<T> e1 = tri.p2 - tri.p1;
		Vec3<T> e2 = tri.p3 - tri.p1;

		return xen::normalized(xen::cross(e2, e1));
	}


	template<u32 T_DIM, typename T>
	LineSegment<T_DIM-1, T> fromHomo(LineSegment<T_DIM, T> a){
		return LineSegment<T_DIM-1, T>{ fromHomo(a.p1), fromHomo(a.p2) };
	}

	template<u32 T_DIM, typename T>
  Triangle<T_DIM-1, T> fromHomo(Triangle<T_DIM, T> a){
		return Triangle<T_DIM-1, T>{ fromHomo(a.p1), fromHomo(a.p2), fromHomo(a.p3) };
	}

	template<u32 T_DIM, typename T>
	LineSegment<T_DIM+1, T> toHomo(LineSegment<T_DIM, T> a, T val = 1){
		return LineSegment<T_DIM+1, T>{ toHomo(a.p1, val), toHomo(a.p2, val) };
	}

	template<u32 T_DIM, typename T>
	Triangle<T_DIM+1, T> toHomo(Triangle<T_DIM, T> a, T val = 1){
		return Triangle<T_DIM+1, T>{ toHomo(a.p1, val), toHomo(a.p2, val), toHomo(a.p3, val) };
	}
}

////////////////////////////////////////////////////////////////////////////////
// Translate by Vector
////////////////////////////////////////////////////////////////////////////////

template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T> operator+(const xen::VertexGroup<T_NUM, T_DIM, T>& vertices,
                                            const xen::Vec<T_DIM, T>& delta){
	xen::VertexGroup<T_NUM, T_DIM, T> result;
	for(u32 i = 0; i < T_NUM; ++i){
		result.vertices[i] = vertices.vertices[i] + delta;
	}
	return result;
}

template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T> operator-(const xen::VertexGroup<T_NUM, T_DIM, T>& vertices,
                                            const xen::Vec<T_DIM, T>& delta){
	xen::VertexGroup<T_NUM, T_DIM, T> result;
	for(u32 i = 0; i < T_NUM; ++i){
		result.vertices[i] = vertices.vertices[i] - delta;
	}
	return result;
}


template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T>& operator+=(xen::VertexGroup<T_NUM, T_DIM, T>& vertices,
                                              const xen::Vec<T_DIM, T>& delta){
	for(u32 i = 0; i < T_NUM; ++i){
		vertices.vertices[i] += delta;
	}
	return vertices;
}

template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T>& operator-=(xen::VertexGroup<T_NUM, T_DIM, T>& vertices,
                                              const xen::Vec<T_DIM, T>& delta){
	for(u32 i = 0; i < T_NUM; ++i){
		vertices.vertices[i] -= delta;
	}
	return vertices;
}

////////////////////////////////////////////////////////////////////////////////
// Multiply by scalar or matrix
////////////////////////////////////////////////////////////////////////////////

template<u32 T_NUM, u32 T_DIM, typename T, typename T_RHS>
xen::VertexGroup<T_NUM, T_DIM, T>& operator*=(xen::VertexGroup <T_NUM, T_DIM, T>& lhs,
                                              const T_RHS& rhs){
	for(u32 i = 0; i < T_NUM; ++i){
		lhs.vertices[i] *= rhs;
	}
	return lhs;
}

template<u32 T_NUM, u32 T_DIM, typename T, typename T_RHS>
xen::VertexGroup<T_NUM, T_DIM, T> operator*(const xen::VertexGroup <T_NUM, T_DIM, T>& lhs,
                                            const T_RHS& rhs){
	xen::VertexGroup<T_NUM, T_DIM, T> result;
	for(u32 i = 0; i < T_NUM; ++i){
		result.vertices[i] = lhs.vertices[i] * rhs;
	}
	return result;
}

////////////////////////////////////////////////////////////////////////////////
// Divide by scalar
////////////////////////////////////////////////////////////////////////////////

template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T>& operator/=(xen::VertexGroup <T_NUM, T_DIM, T>& lhs,
                                              T rhs){
	for(u32 i = 0; i < T_NUM; ++i){
		lhs.vertices[i] /= rhs;
	}
	return lhs;
}

template<u32 T_NUM, u32 T_DIM, typename T>
xen::VertexGroup<T_NUM, T_DIM, T> operator/(const xen::VertexGroup <T_NUM, T_DIM, T>& lhs,
                                            T rhs){
	xen::VertexGroup<T_NUM, T_DIM, T> result;
	for(u32 i = 0; i < T_NUM; ++i){
		result.vertices[i] = lhs.vertices[i] / rhs;
	}
	return result;
}


#endif
