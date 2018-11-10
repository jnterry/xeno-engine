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

	xen::push_back(buffer,  5);
	xen::push_back(buffer, 10);

	REQUIRE(xen::size    (buffer) == 2);
	REQUIRE(xen::capacity(buffer) == 4);

	SECTION("Pop all"){
	  REQUIRE(xen::peak_front(buffer) ==  5);
	  REQUIRE(xen::peak_back (buffer) == 10);
	  REQUIRE(xen::size      (buffer) ==  2);
	  REQUIRE(xen::capacity  (buffer) ==  4);

	  CHECK(xen::pop_front(buffer) == 5);
	  CHECK(xen::size     (buffer) == 1);
	  CHECK(xen::capacity (buffer) == 4);

	  REQUIRE(xen::peak_front(buffer) == 10);
	  REQUIRE(xen::peak_back (buffer) == 10);
	  REQUIRE(xen::size      (buffer) ==  1);
	  REQUIRE(xen::capacity  (buffer) ==  4);

	  CHECK(xen::pop_front(buffer) == 10);
	  CHECK(xen::size     (buffer) ==  0);
	  CHECK(xen::capacity (buffer) ==  4);
	}

	SECTION("Fill and pop"){
		xen::push_back(buffer, 15);
		xen::push_back(buffer, 20);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::pop_front(buffer) ==  5);
		CHECK(xen::pop_front(buffer) == 10);
		CHECK(xen::pop_front(buffer) == 15);
		CHECK(xen::pop_front(buffer) == 20);

		CHECK  (xen::size    (buffer) == 0);
		CHECK  (xen::capacity(buffer) == 4);
	}

	SECTION("Mixed pop and push"){
		CHECK(xen::pop_front(buffer) == 5);
	  CHECK(xen::size     (buffer) == 1);
	  CHECK(xen::capacity (buffer) == 4);

		xen::push_back(buffer, 15);
		xen::push_back(buffer, 20);
		xen::push_back(buffer, 25);
		REQUIRE(xen::size    (buffer) == 4);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::pop_front(buffer) == 10);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 15);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		xen::push_back(buffer, 30);
		xen::push_back(buffer, 35);
		REQUIRE(xen::size    (buffer)  ==  4);
		REQUIRE(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 20);
		CHECK(xen::size    (buffer)  ==  3);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 25);
		CHECK(xen::size    (buffer)  ==  2);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 30);
		CHECK(xen::size    (buffer)  ==  1);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 35);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}

	SECTION("Clear"){
		xen::clear(buffer);
	  CHECK(xen::size     (buffer) == 0);
	  CHECK(xen::capacity (buffer) == 4);

		xen::push_back(buffer, 15);
		xen::push_back(buffer, 20);
		xen::push_back(buffer, 25);
		REQUIRE(xen::size    (buffer) == 3);
		REQUIRE(xen::capacity(buffer) == 4);

		CHECK(xen::peak_front(buffer) == 15);
		CHECK(xen::peak_back (buffer) == 25);

		CHECK(xen::pop_front (buffer) == 15);
		CHECK(xen::peak_front(buffer) == 20);
		CHECK(xen::peak_back (buffer) == 25);
		CHECK(xen::size      (buffer) ==  2);
		CHECK(xen::capacity  (buffer) ==  4);

		CHECK(xen::pop_front (buffer) == 20);
		CHECK(xen::peak_front(buffer) == 25);
		CHECK(xen::peak_back (buffer) == 25);
		CHECK(xen::size      (buffer) ==  1);
		CHECK(xen::capacity  (buffer) ==  4);

		xen::push_back(buffer, 30);
		xen::push_back(buffer, 35);
		xen::push_back(buffer, 40);
		CHECK(xen::peak_front(buffer) == 25);
		CHECK(xen::peak_back (buffer) == 40);
		REQUIRE(xen::size    (buffer) ==  4);
		REQUIRE(xen::capacity(buffer) ==  4);

		CHECK(xen::pop_front (buffer) == 25);
		CHECK(xen::peak_front(buffer) == 30);
		CHECK(xen::peak_back (buffer) == 40);
		CHECK(xen::size      (buffer) ==  3);
		CHECK(xen::capacity  (buffer) ==  4);

		CHECK(xen::pop_front (buffer) == 30);
		CHECK(xen::peak_front(buffer) == 35);
		CHECK(xen::peak_back (buffer) == 40);
		CHECK(xen::size      (buffer) ==  2);
		CHECK(xen::capacity  (buffer) ==  4);

		CHECK(xen::pop_front(buffer) == 35);
		CHECK(xen::peak_front(buffer) == 40);
		CHECK(xen::peak_back (buffer) == 40);
		CHECK(xen::size    (buffer)  ==  1);
		CHECK(xen::capacity(buffer)  ==  4);

		CHECK(xen::pop_front(buffer) == 40);
		CHECK(xen::size    (buffer)  ==  0);
		CHECK(xen::capacity(buffer)  ==  4);
	}
}

// :TODO: test popping from empty ring buffer -> expect assert
// :TODO: test pushing to full ring buffer -> expect assert
