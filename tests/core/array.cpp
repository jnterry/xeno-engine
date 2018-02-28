#include <xen/math/geometry.hpp>
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
