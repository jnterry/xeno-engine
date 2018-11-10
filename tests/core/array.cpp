////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen array types, including fixed arrays
/// and multi-dimensional arrays
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/array.hpp>
#include <catch.hpp>

TEST_CASE("Initializing and accessing 1d array", "[core][array_types]"){
	xen::Array<int> test;
	int testArray[3] = {5,6,7};
	test.size = 3;
	test.elements = testArray;
	CHECK(test.elements[0] == testArray[0]);
	CHECK(test.elements[1] == testArray[1]);
	CHECK(test.elements[2] == testArray[2]);
}

TEST_CASE("Initializing and accessing 2d array", "[core][array_types]"){
	xen::Array2d<int> test;
	int testArray[4] = {5,6,7,8};
	test.rows = 2;
	test.cols = 2;
	test.elements = testArray;
	CHECK(test[0][0] == testArray[0]);
	CHECK(test[0][1] == testArray[1]);
	CHECK(test[1][0] == testArray[2]);
	CHECK(test[1][1] == testArray[3]);
}

TEST_CASE("Initializing and accessing fixed 1d array", "[core][array_types]"){
	xen::FixedArray<int, 3> test;
	int testArray[3] = {5,6,7};
	test.elements[0] = testArray[0];
	test.elements[1] = testArray[1];
	test.elements[2] = testArray[2];
	CHECK(test.elements[0] == testArray[0]);
	CHECK(test.elements[1] == testArray[1]);
	CHECK(test.elements[2] == testArray[2]);
}
/*
TEST_CASE("Initializing and accessing fixed 2d array", "[core][array_types]"){
	xen::FixedArray2d<int, 2, 2> test;
	int testArray[4] = {5,6,7,8};
	test.elements[0] = testArray[0];
	test.elements[1] = testArray[1];
	test.elements[2] = testArray[2];
	test.elements[3] = testArray[3];
	CHECK(test[0][0] == testArray[0]);
	CHECK(test[0][1] == testArray[1]);
	CHECK(test[1][0] == testArray[2]);
	CHECK(test[1][1] == testArray[3]);
}
*/

TEST_CASE("Stretchy Array", "[core][array_types]"){
	int buffer[5] = {10,11,12,0,0};

	xen::StretchyArray<int> test;
	test.elements = buffer;
	test.size     = 3;
	test.capacity = 5;

	REQUIRE(xen::size(test) == 3);
	CHECK  (test[0] == 10);
	CHECK  (test[1] == 11);
	CHECK  (test[2] == 12);

	SECTION("Push and remove ordered"){
		xen::pushBack(test, 50);
		REQUIRE(xen::size(test) == 4);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);

		xen::pushBack(test, 70);
		REQUIRE(xen::size(test) == 5);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);
		CHECK(test[4] == 70);

		xen::remove(test, 4);
		REQUIRE(xen::size(test) == 4);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);

		xen::remove(test, 1);
		REQUIRE(xen::size(test) == 3);
		CHECK(test[0] == 10);
		CHECK(test[1] == 12);
		CHECK(test[2] == 50);
	}

	SECTION("Push and remove unordered", "[core][array_types]"){
		xen::pushBack(test, 50);
		REQUIRE(xen::size(test) == 4);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);

		xen::pushBack(test, 70);
		REQUIRE(xen::size(test) == 5);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);
		CHECK(test[4] == 70);

		xen::removeUnordered(test, 4);
		REQUIRE(xen::size(test) == 4);
		CHECK(test[0] == 10);
		CHECK(test[1] == 11);
		CHECK(test[2] == 12);
		CHECK(test[3] == 50);

		xen::removeUnordered(test, 1);
		REQUIRE(xen::size(test) == 3);
		CHECK(test[0] == 10);
		CHECK(test[1] == 50);
		CHECK(test[2] == 12);
	}

  SECTION("Clear, push, remove", "[core][array_types]"){
		xen::clear(test);
		REQUIRE(xen::size(test) == 0);

		xen::pushBack(test, 5);
		xen::pushBack(test, 6);
		REQUIRE(xen::size(test) == 2);
		CHECK(test[0] == 5);
		CHECK(test[1] == 6);

		xen::removeUnordered(test, 0);
		REQUIRE(xen::size(test) == 1);
		CHECK(test[0] == 6);

		xen::remove(test, 0);
		REQUIRE(xen::size(test) == 0);
	}

}
