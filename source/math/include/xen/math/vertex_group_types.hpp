////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for storing and manipulating a group of vertices,
/// with specialisations for line segments and triangles
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_VERTEX_GROUP_TYPES_HPP
#define XEN_MATH_VERTEX_GROUP_TYPES_HPP

#include <xen/core/array_types.hpp>
#include <xen/math/vector_types.hpp>

namespace xen{
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a group of T_NUM vertices, each with T_DIM elements
	/////////////////////////////////////////////////////////////////////
 	template <u32 T_NUM, u32 T_DIM, typename T>
  union VertexGroup {
		FixedArray<Vec<T_DIM, T>, T_NUM> vertices;

		VertexGroup() {}
	};

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	template <u32 T_DIM, typename T>
  union VertexGroup<2, T_DIM, T> {
		FixedArray<Vec<T_DIM, T>, 2> vertices;
		struct {
			Vec<T_DIM, T> p1;
			Vec<T_DIM, T> p2;
		};

		VertexGroup() {}
		VertexGroup(const Vec<T_DIM, T>& np1, const Vec<T_DIM, T>& np2) : p1(np1), p2(np2) {}
	};

	template <u32 T_DIM, typename T>
	union VertexGroup<3, T_DIM, T> {
		FixedArray<Vec<T_DIM, T>, 3> vertices;
		struct {
			Vec<T_DIM, T> p1;
			Vec<T_DIM, T> p2;
			Vec<T_DIM, T> p3;
		};

		VertexGroup() {}
		VertexGroup(const Vec<T_DIM, T>& np1, const Vec<T_DIM, T>& np2, const Vec<T_DIM, T>& np3) : p1(np1), p2(np2), p3(np3) {}
	};

	template <u32 T_DIM, typename T>
	union VertexGroup<4, T_DIM, T> {
		FixedArray<Vec<T_DIM, T>, 4> vertices;
		struct {
			Vec<T_DIM, T> p1;
			Vec<T_DIM, T> p2;
			Vec<T_DIM, T> p3;
			Vec<T_DIM, T> p4;
		};

		VertexGroup() {}
		VertexGroup(const Vec<T_DIM, T>& np1, const Vec<T_DIM, T>& np2, const Vec<T_DIM, T>& np3, const Vec<T_DIM, T>& np4) : p1(np1), p2(np2), p3(np3), p4(np4) {}
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic

	/////////////////////////////////////////////////////////////////////
	/// \brief Typedefs for arbitary size vertex groups
	/////////////////////////////////////////////////////////////////////
	template <u32 T_NUM, typename T> using VertexGroup2 = VertexGroup<T_NUM, 2, T>;
	template <u32 T_NUM, typename T> using VertexGroup3 = VertexGroup<T_NUM, 3, T>;
	template <u32 T_NUM, typename T> using VertexGroup4 = VertexGroup<T_NUM, 4, T>;
	template <u32 T_NUM> using VertexGroup2r = VertexGroup2<T_NUM,   real>;
	template <u32 T_NUM> using VertexGroup2u = VertexGroup2<T_NUM,    u32>;
	template <u32 T_NUM> using VertexGroup2s = VertexGroup2<T_NUM,    s32>;
	template <u32 T_NUM> using VertexGroup2f = VertexGroup2<T_NUM,  float>;
	template <u32 T_NUM> using VertexGroup2d = VertexGroup2<T_NUM, double>;
	template <u32 T_NUM> using VertexGroup3r = VertexGroup3<T_NUM,   real>;
	template <u32 T_NUM> using VertexGroup3u = VertexGroup3<T_NUM,    u32>;
	template <u32 T_NUM> using VertexGroup3s = VertexGroup3<T_NUM,    s32>;
	template <u32 T_NUM> using VertexGroup3f = VertexGroup3<T_NUM,  float>;
	template <u32 T_NUM> using VertexGroup3d = VertexGroup3<T_NUM, double>;
	template <u32 T_NUM> using VertexGroup4r = VertexGroup4<T_NUM,   real>;
	template <u32 T_NUM> using VertexGroup4u = VertexGroup4<T_NUM,    u32>;
	template <u32 T_NUM> using VertexGroup4s = VertexGroup4<T_NUM,    s32>;
	template <u32 T_NUM> using VertexGroup4f = VertexGroup4<T_NUM,  float>;
	template <u32 T_NUM> using VertexGroup4d = VertexGroup4<T_NUM, double>;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a LineSegment in some number of dimensions
	/////////////////////////////////////////////////////////////////////
	template <u32 T_DIM, typename T> using LineSegment  = VertexGroup<2, T_DIM, T>;
	template <           typename T> using LineSegment2 = LineSegment<2, T>;
	template <           typename T> using LineSegment3 = LineSegment<3, T>;
	template <           typename T> using LineSegment4 = LineSegment<4, T>;
	typedef LineSegment<2, real  > LineSegment2r;
	typedef LineSegment<2, u32   > LineSegment2u;
	typedef LineSegment<2, s32   > LineSegment2s;
	typedef LineSegment<2, float > LineSegment2f;
	typedef LineSegment<2, double> LineSegment2d;
	typedef LineSegment<3, real  > LineSegment3r;
	typedef LineSegment<3, u32   > LineSegment3u;
	typedef LineSegment<3, s32   > LineSegment3s;
	typedef LineSegment<3, float > LineSegment3f;
	typedef LineSegment<3, double> LineSegment3d;
	typedef LineSegment<4, real  > LineSegment4r;
	typedef LineSegment<4, u32   > LineSegment4u;
	typedef LineSegment<4, s32   > LineSegment4s;
	typedef LineSegment<4, float > LineSegment4f;
	typedef LineSegment<4, double> LineSegment4d;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a Triangle in some number of dimensions
	/////////////////////////////////////////////////////////////////////
	template <u32 T_DIM, typename T> using Triangle  = VertexGroup<3, T_DIM, T>;
	template <           typename T> using Triangle2 = Triangle<2, T>;
	template <           typename T> using Triangle3 = Triangle<3, T>;
	template <           typename T> using Triangle4 = Triangle<4, T>;
	typedef Triangle<2, u32   > Triangle2u;
	typedef Triangle<2, s32   > Triangle2s;
	typedef Triangle<2, real  > Triangle2r;
	typedef Triangle<2, float > Triangle2f;
	typedef Triangle<2, double> Triangle2d;
	typedef Triangle<3, u32   > Triangle3u;
	typedef Triangle<3, s32   > Triangle3s;
	typedef Triangle<3, real  > Triangle3r;
	typedef Triangle<3, float > Triangle3f;
	typedef Triangle<3, double> Triangle3d;
	typedef Triangle<4, u32   > Triangle4u;
	typedef Triangle<4, s32   > Triangle4s;
	typedef Triangle<4, real  > Triangle4r;
	typedef Triangle<4, float > Triangle4f;
	typedef Triangle<4, double> Triangle4d;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents a Quad in some number of dimensions
	/////////////////////////////////////////////////////////////////////
	template <u32 T_DIM, typename T> using Quad  = VertexGroup<4, T_DIM, T>;
	template <           typename T> using Quad2 = Quad<2, T>;
	template <           typename T> using Quad3 = Quad<3, T>;
	template <           typename T> using Quad4 = Quad<4, T>;
	typedef Quad<2, u32   > Quad2u;
	typedef Quad<2, s32   > Quad2s;
	typedef Quad<2, real  > Quad2r;
	typedef Quad<2, float > Quad2f;
	typedef Quad<2, double> Quad2d;
	typedef Quad<3, u32   > Quad3u;
	typedef Quad<3, s32   > Quad3s;
	typedef Quad<3, real  > Quad3r;
	typedef Quad<3, float > Quad3f;
	typedef Quad<3, double> Quad3d;
	typedef Quad<4, u32   > Quad4u;
	typedef Quad<4, s32   > Quad4s;
	typedef Quad<4, real  > Quad4r;
	typedef Quad<4, float > Quad4f;
	typedef Quad<4, double> Quad4d;
}

template<u32 T_DIM, typename T>
bool operator==(const xen::LineSegment<T_DIM, T>& lhs, const xen::LineSegment<T_DIM, T>& rhs){
	return lhs.p1 == rhs.p1 && lhs.p2 == rhs.p2;
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::LineSegment<T_DIM, T>& lhs, const xen::LineSegment<T_DIM, T>& rhs){
	return lhs.p1 != rhs.p1 || lhs.p2 != rhs.p2;
}

template<u32 T_DIM, typename T>
bool operator==(const xen::Triangle<T_DIM, T>& lhs, const xen::Triangle<T_DIM, T>& rhs){
	return (lhs.p1 == rhs.p1 &&
	        lhs.p2 == rhs.p2 &&
	        lhs.p3 == rhs.p3
	       );
}

template<u32 T_DIM, typename T>
bool operator!=(const xen::Triangle<T_DIM, T>& lhs, const xen::Triangle<T_DIM, T>& rhs){
	return (lhs.p1 != rhs.p1 ||
	        lhs.p2 != rhs.p2 ||
	        lhs.p3 != rhs.p3
	       );
}

template <u32 T_NUM, u32 T_DIM, typename T>
bool operator==(const xen::VertexGroup<T_NUM, T_DIM, T>& lhs, const xen::VertexGroup<T_NUM, T_DIM, T>& rhs){
	bool result = true;
	for(u32 i = 0; i < T_NUM; ++i){
		result &= lhs.vertices[0] == rhs.vertices[i];
	}
	return result;
}

template <u32 T_NUM, u32 T_DIM, typename T>
bool operator!=(const xen::VertexGroup<T_NUM, T_DIM, T>& lhs, const xen::VertexGroup<T_NUM, T_DIM, T>& rhs){
	bool result = false;
	for(u32 i = 0; i < T_NUM; ++i){
		result |= lhs.vertices[0] != rhs.vertices[i];
	}
	return result;
}

#endif
