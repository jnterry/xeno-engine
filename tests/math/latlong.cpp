////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for latlong
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/math/latlong.hpp>
#include "ostream_operators.hpp"

TEST_CASE("latlong, Cartesian to LatLong", "[math][latlong]"){
	CHECK(xen::toLatLong(Vec3r{ 0,  1,  0}) == xen::LatLong{ 90_deg,  180_deg});
	CHECK(xen::toLatLong(Vec3r{ 0, -1,  0}) == xen::LatLong{-90_deg,  180_deg});

	CHECK(xen::toLatLong(Vec3r{ 1,  0,  0}) == xen::LatLong{  0_deg,    0_deg});
	CHECK(xen::toLatLong(Vec3r{ 0,  0, -1}) == xen::LatLong{  0_deg,   90_deg});
	CHECK(xen::toLatLong(Vec3r{-1,  0,  0}) == xen::LatLong{  0_deg, -180_deg});
	CHECK(xen::toLatLong(Vec3r{ 0,  0,  1}) == xen::LatLong{  0_deg,  -90_deg});
}

TEST_CASE("latlong, LatLong to Cartesian", "[math][latlong]"){
	CHECK(xen::toCartesian(xen::LatLong{   90_deg,    0_deg}) == Vec3r{  0,  1,  0});
	CHECK(xen::toCartesian(xen::LatLong{  -90_deg,    0_deg}) == Vec3r{  0, -1,  0});

	CHECK(xen::toCartesian(xen::LatLong{    0_deg,    0_deg}) == Vec3r{  1,  0,  0});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,   90_deg}) == Vec3r{  0,  0, -1});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  180_deg}) == Vec3r{ -1,  0,  0});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  270_deg}) == Vec3r{  0,  0,  1});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  360_deg}) == Vec3r{  1,  0,  0});

	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  -  0_deg}) == Vec3r{  1,  0,  0});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  - 90_deg}) == Vec3r{  0,  0,  1});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  -180_deg}) == Vec3r{ -1,  0,  0});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  -270_deg}) == Vec3r{  0,  0, -1});
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  -360_deg}) == Vec3r{  1,  0,  0});

	CHECK(xen::toCartesian(xen::LatLong{    0_deg,  -45_deg}) == xen::normalized(Vec3r{ 1, 0,  1}));
	CHECK(xen::toCartesian(xen::LatLong{    0_deg,   45_deg}) == xen::normalized(Vec3r{ 1, 0, -1}));
	CHECK(xen::toCartesian(xen::LatLong{   45_deg,    0_deg}) == xen::normalized(Vec3r{ 1, 1, 0}));
}

TEST_CASE("clamp LatLong", "[math][latlong]"){
	// Test transition points
	// North pole
	CHECK(xen::clamp(xen::LatLong{  85_deg,  45_deg}) == xen::LatLong{  85_deg,   45_deg });
	CHECK(xen::clamp(xen::LatLong{  90_deg,  45_deg}) == xen::LatLong{  90_deg,   45_deg });
	CHECK(xen::clamp(xen::LatLong{  95_deg,  45_deg}) == xen::LatLong{  85_deg, -135_deg });

	// South pole
	CHECK(xen::clamp(xen::LatLong{ -85_deg,  45_deg}) == xen::LatLong{ -85_deg,   45_deg });
	CHECK(xen::clamp(xen::LatLong{ -90_deg,  45_deg}) == xen::LatLong{ -90_deg,   45_deg });
	CHECK(xen::clamp(xen::LatLong{ -95_deg,  45_deg}) == xen::LatLong{ -85_deg, -135_deg });

	// -180/180 singularity
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  179_deg}) == xen::LatLong{ 0_deg,   179_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  180_deg}) == xen::LatLong{ 0_deg,  -180_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  181_deg}) == xen::LatLong{ 0_deg,  -179_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg, -179_deg}) == xen::LatLong{ 0_deg,  -179_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg, -180_deg}) == xen::LatLong{ 0_deg,  -180_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg, -181_deg}) == xen::LatLong{ 0_deg,   179_deg });

	// Large longitude values
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  270_deg}) == xen::LatLong{ 0_deg, - 90_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  360_deg}) == xen::LatLong{ 0_deg,    0_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  450_deg}) == xen::LatLong{ 0_deg,   90_deg });
	CHECK(xen::clamp(xen::LatLong{ 0_deg,  540_deg}) == xen::LatLong{ 0_deg, -180_deg });

	// Large latitude
	CHECK(xen::clamp(xen::LatLong{ 180_deg, 0_deg}) == xen::LatLong{   0_deg, -180_deg });
	CHECK(xen::clamp(xen::LatLong{ 200_deg, 0_deg}) == xen::LatLong{ -20_deg, -180_deg });
	CHECK(xen::clamp(xen::LatLong{ 270_deg, 0_deg}) == xen::LatLong{ -90_deg,    0_deg });
	CHECK(xen::clamp(xen::LatLong{ 300_deg, 0_deg}) == xen::LatLong{ -60_deg,    0_deg });
	CHECK(xen::clamp(xen::LatLong{ 360_deg, 0_deg}) == xen::LatLong{   0_deg,    0_deg });
}

TEST_CASE("latlong, Cartesian -> LatLong -> Cartesian", "[math][latlong]"){
	Vec3r cartesian;
	for(cartesian.x = -10.0_r; cartesian.x <= 10.0_r; cartesian.x += 1.0_r){
		for(cartesian.y = -10.0_r; cartesian.y <= 10.0_r; cartesian.y += 1.0_r){
			for(cartesian.z = -10.0_r; cartesian.z <= 10.0_r; cartesian.z += 1.0_r){
				if(cartesian == Vec3r::Origin){ continue; }

				xen::LatLong ll = xen::toLatLong(cartesian);
				Vec3r output = xen::toCartesian(ll);

				CHECK(xen::normalized(cartesian) == output);
			}
		}
	}
}

/*
TEST_CASE("Project Cube -> Sphere -> Cube", "[math][latlong]"){
	// We construct points on the cube by enforcing that at least 1 of the
	// three components is equal to either 1 or -1 (thus ensuring the point is on
	// the plane containing one of the 6 faces). The other two components vary from
	// -1 to 1 thus ensuring they are within the bounds of the face
	//
	// The following thus maps from the static component id (0 = x, 1 = y, 2 = z)
	// to the 2 components which are variable
	const static u32 VARIABLE_FACES[3][2] = {
		{ 1, 2 },
		{ 0, 2 },
		{ 0, 1 },
	};

	for(u32 fixed_face = 0; fixed_face < 3; ++fixed_face){
		Vec3r cube_coord_pos, cube_coord_neg;
		Vec3r sphr_coord_pos, sphr_coord_neg;

		cube_coord_pos[fixed_face] =  1.0;
		cube_coord_neg[fixed_face] = -1.0;

		for(real a = -1.0; a <= 1.0; a += 0.2){
			for(real b = -1.0; b <= 1.0; b += 0.2){
				cube_coord_pos[VARIABLE_FACES[fixed_face][0]] = a;
				cube_coord_pos[VARIABLE_FACES[fixed_face][1]] = b;
				sphr_coord_pos = xen::projectCubeToSphere(cube_coord_pos);
				std::cout << sphr_coord_pos << std::endl;
				REQUIRE(xen::lengthSq(sphr_coord_pos) == Approx(1.0_r));
				CHECK  (cube_coord_pos == xen::projectSphereToCube(sphr_coord_pos));

				//cube_coord_neg[VARIABLE_FACES[fixed_face][0]] = a;
				//cube_coord_neg[VARIABLE_FACES[fixed_face][1]] = b;
				//sphr_coord_neg = xen::projectCubeToSphere(cube_coord_neg);
				//REQUIRE(xen::lengthSq(sphr_coord_pos) == Approx(1.0_r));
				//CHECK  (cube_coord_neg == xen::projectSphereToCube(sphr_coord_neg));
			}
		}
	}
}
*/
