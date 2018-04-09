////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Includes overloads for piping xen match types to a std::ostream
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <sstream>

#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/math/quaternion_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/math/vertex_group_types.hpp>

#include <catch.hpp>

namespace xen{
	//:TODO:COMP: when do meta type system, have these be auto-generated
	inline std::ostream& operator<< (std::ostream& os, const xen::Angle& a){
		os << xen::asDegrees(a) << "deg";
		return os;
	}

	template<typename T>
	inline std::ostream& operator<< (std::ostream& os, const Vec2<T>& v){
		os << "(" << v.x << "," << v.y << ")";
		return os;
	}

	template<typename T>
	inline std::ostream& operator<< (std::ostream& os, const Vec3<T>& v){
		os << "(" << v.x << "," << v.y << "," << v.z << ")";
		return os;
	}

	template<typename T>
	inline std::ostream& operator<< (std::ostream& os, const Vec4<T>& v){
		os << "(" << v.x << "," << v.y << "," << v.z << "," << v.w << ")";
		return os;
	}

	inline std::ostream& operator<< (std::ostream& os, const xen::AxisAngle& q){
		os << "{" << q.axis << "," << q.angle << "}";
		return os;
	}

	template<u32 T_DIM, typename T>
	inline std::ostream& operator<< (std::ostream& os, const xen::Aabb<T_DIM, T>& aabb){
		os << "Aabb{" << aabb.min << ", " << aabb.max << "}";
		return os;
	}

	template<u32 T_DIM, typename T>
	inline std::ostream& operator<< (std::ostream& os, const xen::LineSegment<T_DIM, T>& line){
		os << "LineSegment{" << line.p1 << ", " << line.p2 << "}";
		return os;
	}

	template<u32 T_DIM, typename T>
	inline std::ostream& operator<< (std::ostream& os, const xen::Ray<T_DIM, T>& ray){
		os << "Ray{" << ray.origin << ", " << ray.direction << "}";
		return os;
	}

	template<u32 T_DIM, typename T>
	inline std::ostream& operator<< (std::ostream& os, const xen::Triangle<T_DIM, T>& tri){
		os << "Triangle[" << tri.p1 << ", " << tri.p2 << tri.p3 << "]";
		return os;
	}

	inline std::ostream& operator<< (std::ostream& os, const xen::Quaternion& q){
		return os << "(" << q.i << "i, " << q.j << "j, " << q.k << "k, " << q.r << ")";
	}

	template<u32 T_Row, u32 T_Col, typename T>
	inline std::ostream& operator<< (std::ostream& os, const xen::Matrix<T_Row, T_Col, T>& m){
		const T* e = m.elements;
		for(u32 r = 0; r < T_Row; ++r){
			os << "[" << e[T_Row * r + 0];
			for(u32 c = 1; c < T_Col; ++c){
				os << ", " << e[T_Row * r + c];
			}
			os << "]";
			if(r != T_Row - 1){ os << "\n"; }
		}
		return os;
	}
}

template<u32 T_Row, u32 T_Col, typename T>
class MatrixMatcher : public Catch::MatcherBase<xen::Matrix<T_Row, T_Col, T>>{
	xen::Matrix<T_Row, T_Col, T> expected;
 public:
	MatrixMatcher(const xen::Matrix<T_Row, T_Col, T>& e) : expected(e) {}
	virtual bool match(xen::Matrix<T_Row, T_Col, T> const& m) const override {
		for(u32 i = 0; i < T_Row * T_Col; ++i){
			if(abs(m.elements[i] - expected.elements[i]) > 0.00001_r){
				return false;
			}
		}
		return true;
	}

	virtual std::string describe() const override {
		std::ostringstream ss;
		ss << " =\n" << expected;
		return ss.str();
	}
};

template<u32 T_Row, u32 T_Col, typename T>
inline MatrixMatcher<T_Row, T_Col, T> IsMat(const xen::Matrix<T_Row, T_Col, T>& expected){
	return MatrixMatcher<T_Row, T_Col, T>(expected);
}
