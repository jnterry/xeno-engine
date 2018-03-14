#include <xen/core/intrinsics.hpp>
#include <catch.hpp>

TEST_CASE("2 way min/max on primitives", "[core][minmax]"){
	CHECK(xen::min(1,  2) ==  1);
	CHECK(xen::min(1, -2) == -2);
	CHECK(xen::min(1,  1) ==  1);
	CHECK(xen::max(1,  2) ==  2);
	CHECK(xen::max(1, -2) ==  1);
	CHECK(xen::max(1,  1) ==  1);

	CHECK(xen::min(10_r, 20_r) == 10_r);
	CHECK(xen::max(10_r, 20_r) == 20_r);
}

TEST_CASE("3 way min/max on primitives", "[core][minmax]"){
	CHECK(xen::min(1, 2, 3) ==  1);
	CHECK(xen::min(2, 1, 3) ==  1);
	CHECK(xen::min(2, 3, 1) ==  1);
	CHECK(xen::min(3, 2, 1) ==  1);

	CHECK(xen::max(1, 2, 3) ==  3);
	CHECK(xen::max(2, 1, 3) ==  3);
	CHECK(xen::max(2, 3, 1) ==  3);
	CHECK(xen::max(3, 2, 1) ==  3);
}

TEST_CASE("4 way min/max on primitives", "[core][minmax]"){
	CHECK(xen::min(1, 2, 3, 4) ==  1);
	CHECK(xen::min(2, 1, 3, 4) ==  1);
	CHECK(xen::min(2, 3, 1, 4) ==  1);
	CHECK(xen::min(2, 3, 4, 1) ==  1);

	CHECK(xen::max(1, 2, 3, 4) ==  4);
	CHECK(xen::max(2, 1, 3, 4) ==  4);
	CHECK(xen::max(2, 3, 1, 4) ==  4);
	CHECK(xen::max(2, 3, 4, 1) ==  4);
}
