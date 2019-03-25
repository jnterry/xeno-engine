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
