////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen ring buffer
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/ring_buffer.hpp>
#include <catch.hpp>

TEST_CASE("Standard ring buffer usage", "[core][ring_buffer]"){
	xen::RingBuffer<int> buffer = {0};
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

// :TODO: test popping from empty ring buffer -> expect assert
// :TODO: test pushing to full ring buffer -> expect assert
