////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types for representing vectors (in the mathematical sense)
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_VECTOR_TYPES_HPP
#define XEN_MATH_VECTOR_TYPES_HPP

#include <xen/core/intrinsics.hpp>

////////////////////////////////////////////////////////////////////////////////
// Define the types
namespace xen{

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

	template<u32 T_DIM, typename T>
	struct Vec{
		T elements[T_DIM];

		T&       operator[](u32 index)       { return this->elements[index]; }
		const T& operator[](u32 index) const { return this->elements[index]; }
	};

	template<typename T>
	struct Vec<2,T>{
		union{
			T elements[2];
			struct{ T x, y; };
			struct{ T u, v; };
		};
		static const Vec<2, T> UnitX, UnitY, Origin;
		static const Vec<2, T> UnitAxes[2];

		template<typename T2>
		explicit operator Vec<2, T2>() const {
			return { (T2)x, (T2)y };
		}

		T&       operator[](u32 index)       { return this->elements[index]; }
		const T& operator[](u32 index) const { return this->elements[index]; }
	};
	template<typename T> const Vec<2, T> Vec<2, T>::UnitX       = {1,0};
	template<typename T> const Vec<2, T> Vec<2, T>::UnitY       = {0,1};
	template<typename T> const Vec<2, T> Vec<2, T>::Origin      = {0,0};
	template<typename T> const Vec<2, T> Vec<2, T>::UnitAxes[2] = {{1,0}, {0,1}};

	template<typename T>
	struct Vec<3,T>{
		Vec<3,T>(){}
		Vec<3,T>(T nx, T ny, T nz) : x(nx), y(ny), z(nz) {}
		union{
			T elements[3];
			struct{ T x, y, z;                     };
			struct{ T u, v;                        };
			struct{ Vec<2, T> uv;                  };
			struct{ Vec<2, T> xy;                  };
			struct{ T _unused1; Vec<2, T> yz; };
		};
		static const Vec<3, T> UnitX, UnitY, UnitZ, Origin;
		static const Vec<3, T> UnitAxes[3];

		template<typename T2>
		explicit operator Vec<3, T2>() const {
			return { (T2)x, (T2)y, (T2)z };
		}

		T&       operator[](u32 index)       { return this->elements[index]; }
		const T& operator[](u32 index) const { return this->elements[index]; }
	};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitX       = {1,0,0};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitY       = {0,1,0};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitZ       = {0,0,1};
	template<typename T> const Vec<3, T> Vec<3, T>::Origin      = {0,0,0};
	template<typename T> const Vec<3, T> Vec<3, T>::UnitAxes[3] = {{1,0,0}
	                                                              ,{0,1,0}
	                                                              ,{0,0,1}
	                                                              };

	template<typename T>
	struct Vec<4,T>{
		Vec<4,T>(T nx, T ny, T nz, T nw) : x(nx), y(ny), z(nz), w(nw){}
		union{
		    T elements[4];
			struct{ T x, y, z, w;                   };
			struct{ T u, v;                         };
			struct{ Vec<2, T> xy, zw;               };
			struct{ Vec<2, T> uv;                   };
			struct{ Vec<3, T> xyz;                  };
			struct{ T _unused1; Vec<3, T> yzw; };
			struct{ T _unused2; Vec<2, T> yz;  };
		};
		static const Vec<4, T> UnitX, UnitY, UnitZ, UnitW, Origin;
		static const Vec<4, T> UnitAxes[4];

		template<typename T2>
		explicit operator Vec<4, T2>() const {
			return { (T2)x, (T2)y, (T2)z, (T2)w };
		}

		T&       operator[](u32 index)       { return this->elements[index]; }
		const T& operator[](u32 index) const { return this->elements[index]; }
	};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitX       = {1,0,0,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitY       = {0,1,0,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitZ       = {0,0,1,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitW       = {0,0,0,1};
	template<typename T> const Vec<4, T> Vec<4, T>::Origin      = {0,0,0,0};
	template<typename T> const Vec<4, T> Vec<4, T>::UnitAxes[4] = {{1,0,0,0}
	                                                              ,{0,1,0,0}
	                                                              ,{0,0,1,0}
	                                                              ,{0,0,0,1}
	                                                              };

	#pragma GCC diagnostic pop // re-enable -Wpedantic
}


////////////////////////////////////////////////////////////////////////////////
// Add typedef shortenings
template<typename T> using Vec2 = xen::Vec<2,T>;
template<typename T> using Vec3 = xen::Vec<3,T>;
template<typename T> using Vec4 = xen::Vec<4,T>;
typedef Vec2<u32>  Vec2u;
typedef Vec2<s32>  Vec2s;
typedef Vec2<r32>  Vec2f;
typedef Vec2<r64>  Vec2d;
typedef Vec2<real> Vec2r;
typedef Vec3<u32>  Vec3u;
typedef Vec3<s32>  Vec3s;
typedef Vec3<r32>  Vec3f;
typedef Vec3<r64>  Vec3d;
typedef Vec3<real> Vec3r;
typedef Vec4<u32>  Vec4u;
typedef Vec4<s32>  Vec4s;
typedef Vec4<r32>  Vec4f;
typedef Vec4<r64>  Vec4d;
typedef Vec4<real> Vec4r;

////////////////////////////////////////////////////////////////////////////////
// Define named constructors
namespace xen {
	/////////////////////////////////////////////////////////////////////
	/// \brief Creates a new xen::Vec from given components
	/// \note We want Vec to be a trivial type (IE: no-constructor) so
	/// we can use it in unions, create with initialise lists, etc, however
	/// sometimes we need to create a vector from more complex data, eg, a 3d
	/// vector from a 2d and a z component. This function allows for this,
	/// without introducing a constructor into Vec
	/////////////////////////////////////////////////////////////////////
	template<typename T> Vec<2, T> mkVec(T x, T y          ){ return {x,y    }; }
	template<typename T> Vec<3, T> mkVec(T x, T y, T z     ){ return {x,y,z  }; }
	template<typename T> Vec<4, T> mkVec(T x, T y, T z, T w){ return {x,y,z,w}; }
	template<typename T>
	Vec<3, T> mkVec(Vec<2, T> xy,  T z) { return { xy.x,  xy.y,  z        }; }
	template<typename T>
	Vec<4, T> mkVec(Vec<3, T> xyz, T w) { return { xyz.x, xyz.y, xyz.z, w }; }
}

////////////////////////////////////////////////////////////////////////////////
// Define comparison operators
template<typename T>
bool operator==(const xen::Vec<2, T>& lhs, const xen::Vec<2, T>& rhs){
	return lhs.x == rhs.x && lhs.y == rhs.y;
}
template<typename T>
bool operator==(const xen::Vec<3, T>& lhs, const xen::Vec<3, T>& rhs){
	return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z;
}
template<typename T>
bool operator==(const xen::Vec<4, T>& lhs, const xen::Vec<4, T>& rhs){
	return lhs.x == rhs.x && lhs.y == rhs.y && lhs.z == rhs.z && lhs.w == rhs.w;
}
template<u32 T_DIM, typename T>
bool operator!=(const xen::Vec<T_DIM, T>& lhs, const xen::Vec<T_DIM, T>& rhs){
	return !(lhs == rhs);
}

#endif
