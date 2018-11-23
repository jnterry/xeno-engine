////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen ring buffer
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/RollingArray.hpp>
#include <catch.hpp>

TEST_CASE("RollingArray as a Standard Queue", "[core][RollingArray]"){
	xen::RollingArray<int> buffer = {0};
	int buffer_memory[4];
	buffer.capacity = XenArrayLength(buffer_memory);
	buffer.elements = buffer_memory;

	REQUIRE(xen::size    (buffer) == 0);
	REQUIRE(xen::capacity(buffer) == 4);

	xen::pushBack(buffer,  5);
	xen::pushBack(buffer, 10);

	REQUIRE(xen::size    (buffer) == 2);
	REQUIRE(xen::capacity(buffer) == 4);

	SECTION("Pop all"){
	  REQUIRE(xen::peakFront(buffer) ==  5);
	  REQUIRE(xen::peakBack (buffer) == 10);
	  REQUIRE(xen::size     (buffer) ==  2);
	  REQUIRE(xen::capacity (buffer) ==  4);

	  CHECK(xen::popFront(buffer) == 5);
	  CHECK(xen::size     (buffer) == 1);
	  CHECK(xen::capacity (buffer) == 4);

	  REQUIRE(xen::peakFront(buffer) == 10);
	  REQUIRE(xen::peakBack (buffer) == 10);
	  REQUIRE(xen::size     (buffer) ==  1);
	  REQUIRE(xen::capacity (buffer) ==  4);

	  CHECK(xen::popFront(buffer) == 10);
	  CHECK(xen::size    (buffer) ==  0);
	  CHECK(xen::capacity(buffer) ==  4);
	}

	SECTION("Fill and pop"){
		xen::pushBack(buffer, 15);
		xen::pushBack(buffer, 20);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::popFront(buffer) ==  5);
		CHECK(xen::popFront(buffer) == 10);
		CHECK(xen::popFront(buffer) == 15);
		CHECK(xen::popFront(buffer) == 20);

		CHECK  (xen::size    (buffer) == 0);
		CHECK  (xen::capacity(buffer) == 4);
	}

	SECTION("Mixed pop and push"){
		CHECK(xen::popFront(buffer) == 5);
	  CHECK(xen::size    (buffer) == 1);
	  CHECK(xen::capacity(buffer) == 4);

		xen::pushBack(buffer, 15);
		xen::pushBack(buffer, 20);
		xen::pushBack(buffer, 25);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::popFront(buffer) == 10);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 15);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		xen::pushBack(buffer, 30);
		xen::pushBack(buffer, 35);
		REQUIRE(xen::size    (buffer)  ==  4);
		REQUIRE(xen::capacity(buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 20);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 25);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 30);
		CHECK(xen::size    (buffer)  ==  1);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 35);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}

	SECTION("Clear"){
		xen::clear(buffer);
	  CHECK(xen::size     (buffer) == 0);
	  CHECK(xen::capacity (buffer) == 4);

		xen::pushBack(buffer, 15);
		xen::pushBack(buffer, 20);
		xen::pushBack(buffer, 25);
		REQUIRE(xen::size    (buffer) == 3);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::peakFront (buffer) == 15);
		CHECK(xen::peakBack  (buffer) == 25);

		CHECK(xen::popFront  (buffer) == 15);
		CHECK(xen::peakFront (buffer) == 20);
		CHECK(xen::peakBack  (buffer) == 25);
		CHECK(xen::size      (buffer) ==  2);
		CHECK(xen::capacity  (buffer) ==  4);

		CHECK(xen::popFront (buffer) == 20);
		CHECK(xen::peakFront(buffer) == 25);
		CHECK(xen::peakBack (buffer) == 25);
		CHECK(xen::size     (buffer) ==  1);
		CHECK(xen::capacity (buffer) ==  4);

		xen::pushBack(buffer, 30);
		xen::pushBack(buffer, 35);
		xen::pushBack(buffer, 40);
		CHECK(xen::peakFront (buffer) == 25);
		CHECK(xen::peakBack  (buffer) == 40);
		REQUIRE(xen::size    (buffer) ==  4);
		REQUIRE(xen::capacity(buffer) ==  4);

		CHECK(xen::popFront (buffer) == 25);
		CHECK(xen::peakFront(buffer) == 30);
		CHECK(xen::peakBack (buffer) == 40);
		CHECK(xen::size     (buffer) ==  3);
		CHECK(xen::capacity (buffer) ==  4);

		CHECK(xen::popFront (buffer) == 30);
		CHECK(xen::peakFront(buffer) == 35);
		CHECK(xen::peakBack (buffer) == 40);
		CHECK(xen::size     (buffer) ==  2);
		CHECK(xen::capacity (buffer) ==  4);

		CHECK(xen::popFront (buffer) == 35);
		CHECK(xen::peakFront(buffer) == 40);
		CHECK(xen::peakBack (buffer) == 40);
		CHECK(xen::size     (buffer)  ==  1);
		CHECK(xen::capacity (buffer)  ==  4);

		CHECK(xen::popFront(buffer) == 40);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}
}

TEST_CASE("RollingArray as a Reverse Queue", "[core][RollingArray]"){
	xen::RollingArray<int> buffer = {0};
	int buffer_memory[4];
	buffer.capacity = XenArrayLength(buffer_memory);
	buffer.elements = buffer_memory;

	REQUIRE(xen::size    (buffer) == 0);
	REQUIRE(xen::capacity(buffer) == 4);

	xen::pushFront(buffer,  5);
	xen::pushFront(buffer, 10);

	REQUIRE(xen::size    (buffer) == 2);
	REQUIRE(xen::capacity(buffer) == 4);

	SECTION("Pop all"){
	  REQUIRE(xen::peakBack(buffer) ==  5);
	  REQUIRE(xen::peakFront (buffer) == 10);
	  REQUIRE(xen::size     (buffer) ==  2);
	  REQUIRE(xen::capacity (buffer) ==  4);

	  CHECK(xen::popBack(buffer) == 5);
	  CHECK(xen::size     (buffer) == 1);
	  CHECK(xen::capacity (buffer) == 4);

	  REQUIRE(xen::peakBack(buffer) == 10);
	  REQUIRE(xen::peakFront (buffer) == 10);
	  REQUIRE(xen::size     (buffer) ==  1);
	  REQUIRE(xen::capacity (buffer) ==  4);

	  CHECK(xen::popBack(buffer) == 10);
	  CHECK(xen::size    (buffer) ==  0);
	  CHECK(xen::capacity(buffer) ==  4);
	}

	SECTION("Fill and pop"){
		xen::pushFront(buffer, 15);
		xen::pushFront(buffer, 20);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::popBack(buffer) ==  5);
		CHECK(xen::popBack(buffer) == 10);
		CHECK(xen::popBack(buffer) == 15);
		CHECK(xen::popBack(buffer) == 20);

		CHECK  (xen::size    (buffer) == 0);
		CHECK  (xen::capacity(buffer) == 4);
	}

	SECTION("Mixed pop and push"){
		CHECK(xen::popBack(buffer) == 5);
	  CHECK(xen::size    (buffer) == 1);
	  CHECK(xen::capacity(buffer) == 4);

		xen::pushFront(buffer, 15);
		xen::pushFront(buffer, 20);
		xen::pushFront(buffer, 25);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::popBack(buffer) == 10);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 15);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		xen::pushFront(buffer, 30);
		xen::pushFront(buffer, 35);
		REQUIRE(xen::size    (buffer)  ==  4);
		REQUIRE(xen::capacity(buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 20);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 25);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 30);
		CHECK(xen::size    (buffer)  ==  1);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 35);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}

	SECTION("Clear"){
		xen::clear(buffer);
	  CHECK(xen::size    (buffer) == 0);
	  CHECK(xen::capacity(buffer) == 4);

		xen::pushFront(buffer, 15);
		xen::pushFront(buffer, 20);
		xen::pushFront(buffer, 25);
		REQUIRE(xen::size    (buffer) == 3);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::peakBack (buffer) == 15);
		CHECK(xen::peakFront(buffer) == 25);

		CHECK(xen::popBack  (buffer) == 15);
		CHECK(xen::peakBack (buffer) == 20);
		CHECK(xen::peakFront(buffer) == 25);
		CHECK(xen::size     (buffer) ==  2);
		CHECK(xen::capacity (buffer) ==  4);

		CHECK(xen::popBack (buffer) == 20);
		CHECK(xen::peakBack(buffer) == 25);
		CHECK(xen::peakFront (buffer) == 25);
		CHECK(xen::size     (buffer) ==  1);
		CHECK(xen::capacity (buffer) ==  4);

		xen::pushFront(buffer, 30);
		xen::pushFront(buffer, 35);
		xen::pushFront(buffer, 40);
		CHECK(xen::peakBack (buffer) == 25);
		CHECK(xen::peakFront  (buffer) == 40);
		REQUIRE(xen::size    (buffer) ==  4);
		REQUIRE(xen::capacity(buffer) ==  4);

		CHECK(xen::popBack (buffer) == 25);
		CHECK(xen::peakBack(buffer) == 30);
		CHECK(xen::peakFront (buffer) == 40);
		CHECK(xen::size     (buffer) ==  3);
		CHECK(xen::capacity (buffer) ==  4);

		CHECK(xen::popBack (buffer) == 30);
		CHECK(xen::peakBack(buffer) == 35);
		CHECK(xen::peakFront (buffer) == 40);
		CHECK(xen::size     (buffer) ==  2);
		CHECK(xen::capacity (buffer) ==  4);

		CHECK(xen::popBack (buffer) == 35);
		CHECK(xen::peakBack(buffer) == 40);
		CHECK(xen::peakFront (buffer) == 40);
		CHECK(xen::size     (buffer)  ==  1);
		CHECK(xen::capacity (buffer)  ==  4);

		CHECK(xen::popBack(buffer) == 40);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}
}

TEST_CASE("RollingArray as an Array", "[core][RollingArray]"){
	xen::RollingArray<int> buffer = {0};
	int buffer_memory[4];
	buffer.capacity = XenArrayLength(buffer_memory);
	buffer.elements = buffer_memory;

	REQUIRE(xen::size    (buffer) == 0);
	REQUIRE(xen::capacity(buffer) == 4);

	xen::pushBack(buffer,  5);
	xen::pushBack(buffer, 10);

	SECTION("No wrap"){
		REQUIRE(xen::isIndexValid(buffer, 0) ==  true);
		CHECK  (                  buffer [0] ==     5);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  true);
		CHECK  (                  buffer [1] ==    10);
		CHECK  (xen::isIndexValid(buffer, 2) == false);

		CHECK(xen::popFront(buffer) == 5);

	  REQUIRE(xen::isIndexValid(buffer, 0) == false);
	  REQUIRE(xen::isIndexValid(buffer, 1) == true );
	  CHECK  (                  buffer [1] ==    10);
	  CHECK  (xen::isIndexValid(buffer, 2) == false);

	  CHECK(xen::popFront(buffer) == 10);
	  CHECK(xen::isIndexValid(buffer, 0) == false);
	  CHECK(xen::isIndexValid(buffer, 1) == false);
	  CHECK(xen::isIndexValid(buffer, 2) == false);
	  CHECK(xen::isEmpty (buffer)        ==  true);
	  CHECK(xen::size    (buffer)        ==     0);
	  CHECK(xen::capacity(buffer)        ==     4);
	}

	SECTION("With Wrap") {
		xen::pushBack(buffer, 15);
		xen::pushBack(buffer, 20);
		xen::pushBack(buffer, 25);
		xen::pushBack(buffer, 30);

		CHECK  (xen::size    (buffer)        ==     4);
	  CHECK  (xen::capacity(buffer)        ==     4);

		REQUIRE(xen::isIndexValid(buffer, 0) == false);
		REQUIRE(xen::isIndexValid(buffer, 1) == false);
		REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		CHECK  (                  buffer [2] ==    15);
		REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		CHECK  (                  buffer [3] ==    20);
		REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
		CHECK  (                  buffer [4] ==    25);
		REQUIRE(xen::isIndexValid(buffer, 5) ==  true);
		CHECK  (                  buffer [5] ==    30);
		REQUIRE(xen::isIndexValid(buffer, 6) == false);
	}


	SECTION("Multiple wraps") {
		xen::pushBack(buffer, 15);
		xen::pushBack(buffer, 20);

		CHECK  (xen::size    (buffer)        ==     4);
	  CHECK  (xen::capacity(buffer)        ==     4);

		REQUIRE(xen::isIndexValid(buffer, 0) ==  true);
		CHECK  (                  buffer [0] ==     5);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  true);
		CHECK  (                  buffer [1] ==    10);
		REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		CHECK  (                  buffer [2] ==    15);
		REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		CHECK  (                  buffer [3] ==    20);
		REQUIRE(xen::isIndexValid(buffer, 4) ==  false);

		xen::popBack (buffer);
		xen::popFront(buffer);
		xen::popFront(buffer);

		REQUIRE(xen::isIndexValid(buffer, 0) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 2) ==   true);
		CHECK  (                  buffer [2] ==     15);
		REQUIRE(xen::isIndexValid(buffer, 3) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 5) ==  false);

		xen::pushBack(buffer, 100);
		xen::pushBack(buffer, 200);
		xen::pushBack(buffer, 300);

		REQUIRE(xen::isIndexValid(buffer, 0) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 2) ==   true);
		CHECK  (                  buffer [2] ==     15);
		REQUIRE(xen::isIndexValid(buffer, 3) ==   true);
		CHECK  (                  buffer [3] ==    100);
		REQUIRE(xen::isIndexValid(buffer, 4) ==   true);
		CHECK  (                  buffer [4] ==    200);
		REQUIRE(xen::isIndexValid(buffer, 5) ==   true);
		CHECK  (                  buffer [5] ==    300);
		REQUIRE(xen::isIndexValid(buffer, 6) ==  false);

		xen::pushBack(buffer, 400);
		xen::pushBack(buffer, 500);
		xen::pushBack(buffer, 600);
		xen::pushBack(buffer, 700);

		REQUIRE(xen::isIndexValid(buffer, 0) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 2) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 3) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 4) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 5) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 6) ==   true);
		CHECK  (                  buffer [6] ==    400);
		REQUIRE(xen::isIndexValid(buffer, 7) ==   true);
		CHECK  (                  buffer [7] ==    500);
		REQUIRE(xen::isIndexValid(buffer, 8) ==   true);
		CHECK  (                  buffer [8] ==    600);
		REQUIRE(xen::isIndexValid(buffer, 9) ==   true);
		CHECK  (                  buffer [9] ==    700);
		REQUIRE(xen::isIndexValid(buffer,10) ==  false);

		xen::pushFront(buffer, 999);
		xen::pushFront(buffer, 888);

		REQUIRE(xen::isIndexValid(buffer, 0) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 1) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 2) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 3) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 4) ==   true);
		CHECK  (                  buffer [4] ==    888);
		REQUIRE(xen::isIndexValid(buffer, 5) ==   true);
		CHECK  (                  buffer [5] ==    999);
		REQUIRE(xen::isIndexValid(buffer, 6) ==   true);
		CHECK  (                  buffer [6] ==    400);
		REQUIRE(xen::isIndexValid(buffer, 7) ==   true);
		CHECK  (                  buffer [7] ==    500);
		REQUIRE(xen::isIndexValid(buffer, 8) ==  false);
		REQUIRE(xen::isIndexValid(buffer, 9) ==  false);
		REQUIRE(xen::isIndexValid(buffer,10) ==  false);
	}
}
