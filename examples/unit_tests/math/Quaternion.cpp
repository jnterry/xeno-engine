#include <xen/math/Vector.hpp>
#include <xen/math/Matrix.hpp>
#include <xen/math/Quaternion.hpp>
#include <iostream>

namespace xen{
	//:TODO:COMP: when do meta type system, have these be auto-generated
	std::ostream& operator<< (std::ostream& os, xen::Angle const& a){
		os << xen::asDegrees(a) << "deg";
		return os;
	}
	template<typename T>
	std::ostream& operator<< (std::ostream& os, Vec3<T> const& v){
		os << "(" << v.x << "," << v.y << "," << v.z << ")";
		return os;
	}
	template<typename T>
	std::ostream& operator<< (std::ostream& os, Vec4<T> const& v){
		os << "(" << v.x << "," << v.y << "," << v.z << "," << v.w << ")";
		return os;
	}
	std::ostream& operator<< (std::ostream& os, xen::AxisAngle const& q){
		os << "{" << q.axis << "," << q.angle << "}";
		return os;
	}

	template<u32 T_Row, u32 T_Col, typename T>
	std::ostream& operator<< (std::ostream& os, xen::Matrix<T_Row, T_Col, T> const& m){
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

#include <catch.hpp>

#define COS_45 0.70710678118

TEST_CASE("Making Quaternion from AxisAngle",
          "[math][Quaternion][AxisAngle]"){

	REQUIRE( Quat::Identity == (Quat{0,0,0,1}) );

	SECTION("90 Deg rotation about each axis"){
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitX, 90_deg) == (Quat{COS_45, 0, 0, COS_45}));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitY, 90_deg) == (Quat{0, COS_45, 0, COS_45}));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitZ, 90_deg) == (Quat{0, 0, COS_45, COS_45}));
	}

	SECTION("0 Deg rotation about each axis"){
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitX, 0_deg) == (Quat::Identity));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitY, 0_deg) == (Quat::Identity));
		REQUIRE(xen::fromAxisAngle(Vec3r::UnitZ, 0_deg) == (Quat::Identity));
	}
}


TEST_CASE("AxisAngle -> Quaternion -> AxisAngle results in no change",
          "[math][Quaternion][AxisAngle]"){
	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitX,  30_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitX,  30_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitY, 120_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitY, 120_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r::UnitZ, 250_deg))) ==
	        (xen::AxisAngle{Vec3r::UnitZ, 250_deg}));

	REQUIRE((xen::toAxisAngle(xen::fromAxisAngle(Vec3r{0, 0.5, 1}, 45_deg))) ==
	        (xen::AxisAngle{xen::normalized(Vec3r{0, 0.5, 1}), 45_deg}));
}

TEST_CASE("Rotation Matrix from Quaterion",
          "[math][Quaternion][AxisAngle]"){

	SECTION("Rot X"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitX, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 90_deg))  ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, -90_deg))  ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 270_deg))  ==
			        ( Vec3r{0,  0,  1}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitX, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dx(  0_deg)) == (xen::Rotation3d(Vec3r::UnitX,   0_deg)));
				REQUIRE((xen::Rotation3dx( 90_deg)) == (xen::Rotation3d(Vec3r::UnitX,  90_deg)));
				REQUIRE((xen::Rotation3dx(180_deg)) == (xen::Rotation3d(Vec3r::UnitX, 180_deg)));
				REQUIRE((xen::Rotation3dx(270_deg)) == (xen::Rotation3d(Vec3r::UnitX, 270_deg)));
			}
		}
	}

	SECTION("Rot Y"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitY, 90_deg))  ==
			        ( Vec3r{0,  1,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitY, -90_deg))  ==
			        ( Vec3r{0,  0, -1}));
			REQUIRE(((Vec3r{1,  5,  0}) * xen::Rotation3d(Vec3r::UnitY, 270_deg))  ==
			        ( Vec3r{0,  5, -1}));
			REQUIRE(((Vec3r{0,  0,  1}) * xen::Rotation3d(Vec3r::UnitY, 180_deg))  ==
			        ( Vec3r{0,  0, -1}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dy(  0_deg)) == (xen::Rotation3d(Vec3r::UnitY,   0_deg)));
				REQUIRE((xen::Rotation3dy( 90_deg)) == (xen::Rotation3d(Vec3r::UnitY,  90_deg)));
				REQUIRE((xen::Rotation3dy(180_deg)) == (xen::Rotation3d(Vec3r::UnitY, 180_deg)));
				REQUIRE((xen::Rotation3dy(270_deg)) == (xen::Rotation3d(Vec3r::UnitY, 270_deg)));
			}
		}
	}


	SECTION("Rot Z"){
		GIVEN("Quaternion Rotation works"){
			REQUIRE(((Vec3r{0,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{0,  0,  0}));
			REQUIRE(((Vec3r{0,  0, 10}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{0,  0, 10}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 90_deg))  ==
			        ( Vec3r{1,  0,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, -90_deg))  ==
			        ( Vec3r{0, -1,  0}));
			REQUIRE(((Vec3r{1,  0,  0}) * xen::Rotation3d(Vec3r::UnitZ, 270_deg))  ==
			        ( Vec3r{0, -1,  0}));
			REQUIRE(((Vec3r{0,  1,  0}) * xen::Rotation3d(Vec3r::UnitZ, 180_deg))  ==
			        ( Vec3r{0, -1,  0}));

			THEN("Quaternion Matricies equivalent to Rotation Matricies"){
				REQUIRE((xen::Rotation3dz(  0_deg)) == (xen::Rotation3d(Vec3r::UnitZ,   0_deg)));
				REQUIRE((xen::Rotation3dz( 90_deg)) == (xen::Rotation3d(Vec3r::UnitZ,  90_deg)));
				REQUIRE((xen::Rotation3dz(180_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 180_deg)));
				REQUIRE((xen::Rotation3dz(270_deg)) == (xen::Rotation3d(Vec3r::UnitZ, 270_deg)));
			}
		}
	}
}
