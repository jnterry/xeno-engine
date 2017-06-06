#include <iostream>

#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Quaternion.hpp>

namespace xen{
	//:TODO:COMP: when do meta type system, have these be auto-generated
	inline std::ostream& operator<< (std::ostream& os, xen::Angle const& a){
		os << xen::asDegrees(a) << "deg";
		return os;
	}

	template<typename T>
	inline std::ostream& operator<< (std::ostream& os, Vec3<T> const& v){
		os << "(" << v.x << "," << v.y << "," << v.z << ")";
		return os;
	}

	template<typename T>
	inline std::ostream& operator<< (std::ostream& os, Vec4<T> const& v){
		os << "(" << v.x << "," << v.y << "," << v.z << "," << v.w << ")";
		return os;
	}

	inline std::ostream& operator<< (std::ostream& os, xen::AxisAngle const& q){
		os << "{" << q.axis << "," << q.angle << "}";
		return os;
	}

	inline std::ostream& operator<< (std::ostream& os, xen::Quaternion const& q){
		return os << "(" << q.i << "i, " << q.j << "j, " << q.k << "k, " << q.r << ")";
	}

	template<u32 T_Row, u32 T_Col, typename T>
	inline std::ostream& operator<< (std::ostream& os, xen::Matrix<T_Row, T_Col, T> const& m){
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
