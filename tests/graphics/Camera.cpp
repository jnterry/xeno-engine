#include <xen/graphics/Camera3d.hpp>
#include "../math/ostream_operators.hpp"
#include <catch.hpp>

TEST_CASE("Camera3dCylinder Position", "[graphics][Camera3d][Camera3dCylinder]"){
	xen::Camera3dCylinder camera;
	camera.target = Vec3r::Origin;
	camera.up_dir = Vec3r::UnitY;
	camera.axis   = Vec3r::UnitY;
	camera.height = 0;
	camera.radius = 100;
	camera.angle  = 0_deg;

	camera.angle = 0_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{0, 0, 100});
	camera.angle = 90_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{100, 0, 0});
	camera.angle = 180_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{0, 0, -100});
	camera.angle = 270_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{-100, 0, 0});

	camera.height = 50;

	camera.angle = 0_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{0, 50, 100});
	camera.angle = 90_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{100, 50, 0});
	camera.angle = 180_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{0, 50, -100});
	camera.angle = 270_deg;
	CHECK(xen::getCameraPosition(camera) == Vec3r{-100, 50, 0});
}
