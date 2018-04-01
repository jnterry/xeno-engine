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

#include "impl/swizzles.hxx"

namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the barycentric coordinates for a
	/// point p with respect to a given triangle
	/// Taken from: https://gamedev.stackexchange.com/a/23745
	/// \param p The point to find barycentric coordinates of
	/// \param tri The triangle Barycentric coordinates are with respect to
	/// \return Barycentric coordinates of point p
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	Vec3<T> getBarycentricCoordinates(const Triangle<T_DIM,T>& tri, const Vec<T_DIM,T>& p){
    Vec<T_DIM,T> v0 = tri.p2 - tri.p1;
		Vec<T_DIM,T> v1 = tri.p3 - tri.p1;
		Vec<T_DIM,T> v2 = p - tri.p1;

    T d00 = xen::dot(v0, v0);
    T d01 = xen::dot(v0, v1);
    T d11 = xen::dot(v1, v1);
    T d20 = xen::dot(v2, v0);
    T d21 = xen::dot(v2, v1);
    T denom = d00 * d11 - d01 * d01;
    T v = (d11 * d20 - d01 * d21) / denom;
    T w = (d00 * d21 - d01 * d20) / denom;
    T u = (T)1 - v - w;
		return {u,v,w};
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the surface normal for some triangle
	/// \param tri The triangle whose normal you wish to compute
	/// \return Vec3 representing the normal of the triangle
	/// \public \memberof Triangle
	/////////////////////////////////////////////////////////////////////
	template<typename T>
	Vec3<T> computeTriangleNormal(const Triangle3<T>& tri){
		Vec3<T> u = tri.p2 - tri.p1;
		Vec3<T> v = tri.p3 - tri.p1;

		Vec3<T> normal;
		normal.x = u.y*v.z - u.x*v.y;
		normal.y = u.z*v.x - u.x*v.z;
		normal.z = u.x*v.y - u.y*v.x;

		return normal;
	}

	/*
	  Not currently used. Idea is to avoid floating point issues by ensuring the
	  point is projected onto plane of triangle before computing the barycentric
	  coordinates
	/////////////////////////////////////////////////////////////////////
	/// \brief Computes the barycentric coordinates for the projection p' of
	/// point p into the plane of triangle tri
	/// Taken from: https://pdfs.semanticscholar.org/0141/b1416bb749bb5ba94210a30d70f0824760a4.pdf
	/// \param p The point to find barycentric coordinates of
	/// \param tri The triangle Barycentric coordinates are with respect to
	/// \return Barycentric coordinates of point p
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	Vec3<T> getProjectedBarycentricCoordinates(const Triangle<T_DIM,T>& tri, const Vec<T_DIM,T>& p){
		//:TODO: Translate this into Jamie friendly code
		Vec<T_DIM,T> q = tri.p1;
		Vec<T_DIM,T> u = tri.p2-tri.p1;
		Vec<T_DIM,T> v = tri.p3-tri.p1;

		Vec<T_DIM,T> n = xen::cross( u, v );
		real oneOver4ASquared= 1.0 / xen::dot( n, n );
		Vec<T_DIM,T> w = p - q;

		T y = dot( cross( w, v ), n ) * oneOver4ASquared;
		T z = dot( cross( u, w ), n ) * oneOver4ASquared;
		T x = (T)1 - y - z;

		return {x,y,z};
	}
	*/

	/////////////////////////////////////////////////////////////////////
	/// \brief Evaluates the value of a given barycentric coordinates
	/// of a specific point with respect to a given triangle
	/// \param bary Barycentric coordinates of a known point
	/// \param tri The triangle this point is being resolved within
	/// \return Evalauted quantity, of type T1, of bary with respect to tri
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T1, typename T2>
	Vec<T_DIM,T1> evaluateBarycentricCoordinates(const Triangle<T_DIM,T1>& tri, const Vec3<T2>& bary){
		Vec<T_DIM,T1> toReturn = (bary.x*tri.p1 +
									bary.y*tri.p2 +
									bary.z*tri.p3);
		return toReturn;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Performs a lerp along some line segment, equivalent to
	/// lerping between the line's .p1 to its .p2
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	Vec<T_DIM, T> lerp(LineSegment<T_DIM, T> line, const real factor){
		return xen::lerp(line.p1, line.p2, factor);
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

namespace xen {

	template<char T_S1, char T_S2, u32 T_DIM, typename T>
	LineSegment2<T> swizzle(const LineSegment<T_DIM, T>& l){
		return {
			xen::swizzle<T_S1, T_S2>(l.p1),
			xen::swizzle<T_S1, T_S2>(l.p2),
		};
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
  LineSegment3<T> swizzle(const LineSegment<T_DIM, T>& l){
		return {
			xen::swizzle<T_S1, T_S2, T_S3>(l.p1),
			xen::swizzle<T_S1, T_S2, T_S3>(l.p2),
		};
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
  LineSegment4<T> swizzle(const LineSegment<T_DIM, T>& l){
			return {
				xen::swizzle<T_S1, T_S2, T_S3, T_S4>(l.p1),
				xen::swizzle<T_S1, T_S2, T_S3, T_S4>(l.p2),
			};
	}

	template<char T_S1, char T_S2, u32 T_DIM, typename T>
	Triangle2<T> swizzle(const Triangle<T_DIM, T>& t){
		return {
			xen::swizzle<T_S1, T_S2>(t.p1),
			xen::swizzle<T_S1, T_S2>(t.p2),
			xen::swizzle<T_S1, T_S2>(t.p3),
		};
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_DIM, typename T>
  Triangle3<T> swizzle(const Triangle<T_DIM, T>& t){
		return {
			xen::swizzle<T_S1, T_S2, T_S3>(t.p1),
			xen::swizzle<T_S1, T_S2, T_S3>(t.p2),
			xen::swizzle<T_S1, T_S2, T_S3>(t.p3),
		};
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_DIM, typename T>
  Triangle4<T> swizzle(const Triangle<T_DIM, T>& t){
			return {
				xen::swizzle<T_S1, T_S2, T_S3, T_S4>(t.p1),
				xen::swizzle<T_S1, T_S2, T_S3, T_S4>(t.p2),
				xen::swizzle<T_S1, T_S2, T_S3, T_S4>(t.p3),
			};
	}

	template<char T_S1, char T_S2, u32 T_COUNT, u32 T_DIM, typename T>
	VertexGroup<T_COUNT, 2, T> swizzle(const VertexGroup<T_COUNT, T_DIM, T>& vg){
		VertexGroup<T_COUNT, 2, T> out;
		for(u32 i = 0; i < T_COUNT; ++i){
			out.vertices[i] = xen::swizzle<T_S1, T_S2>(vg.vertices[i]);
		}
		return out;
	}
	template<char T_S1, char T_S2, char T_S3, u32 T_COUNT, u32 T_DIM, typename T>
	VertexGroup<T_COUNT, 3, T> swizzle(const VertexGroup<T_COUNT, T_DIM, T>& vg){
		VertexGroup<T_COUNT, 3, T> out;
		for(u32 i = 0; i < T_COUNT; ++i){
			out.vertices[i] = xen::swizzle<T_S1, T_S2, T_S3>(vg.vertices[i]);
		}
		return out;
	}
	template<char T_S1, char T_S2, char T_S3, char T_S4, u32 T_COUNT, u32 T_DIM, typename T>
	VertexGroup<T_COUNT, 4, T> swizzle(const VertexGroup<T_COUNT, T_DIM, T>& vg){
		VertexGroup<T_COUNT, 4, T> out;
		for(u32 i = 0; i < T_COUNT; ++i){
			out.vertices[i] = xen::swizzle<T_S1, T_S2, T_S3, T_S4>(vg.vertices[i]);
		}
		return out;
	}
}


#endif
