////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::RingBuffer
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/RingBuffer.hpp>
#include <catch.hpp>

TEST_CASE("RingBuffer as a Standard Queue", "[core][RingBuffer]"){
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

TEST_CASE("RingBuffer as a Reverse Queue", "[core][RingBuffer]"){
	xen::RingBuffer<int> buffer = {0};
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

TEST_CASE("RingBuffer as an Array", "[core][RingBuffer]"){
	xen::RingBuffer<int> buffer = {0};
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
		xen::pushBack<int, false>(buffer, 15);
		xen::pushBack<int, false>(buffer, 20);
		xen::pushBack<int, false>(buffer, 25);
		xen::pushBack<int, false>(buffer, 30);

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

		xen::pushBack<int, false>(buffer, 100);
		xen::pushBack<int, false>(buffer, 200);
		xen::pushBack<int, false>(buffer, 300);

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

		xen::pushBack<int, false>(buffer, 400);
		xen::pushBack<int, false>(buffer, 500);
		xen::pushBack<int, false>(buffer, 600);
		xen::pushBack<int, false>(buffer, 700);

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

		xen::pushFront<int, false>(buffer, 999);
		xen::pushFront<int, false>(buffer, 888);

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


	SECTION("Remove unordered"){
		REQUIRE(xen::popFront(buffer) ==  5);
		REQUIRE(xen::popFront(buffer) == 10);

		REQUIRE(xen::size(buffer) == 0);

	  xen::pushBack(buffer, 100);
	  xen::pushBack(buffer, 200);
	  xen::pushBack(buffer, 300);
	  xen::pushBack(buffer, 400);

	  REQUIRE(xen::isIndexValid(buffer, 1) == false);
	  REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
	  REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
	  REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
	  REQUIRE(xen::isIndexValid(buffer, 5) ==  true);
	  REQUIRE(xen::isIndexValid(buffer, 6) == false);
	  REQUIRE(buffer[2] == 100);
	  REQUIRE(buffer[3] == 200);
	  REQUIRE(buffer[4] == 300);
	  REQUIRE(buffer[5] == 400);

	  SECTION("Remove First"){
		  xen::removeUnordered(buffer, 2);

		  CHECK  (xen::size(buffer) == 3);
		  REQUIRE(xen::isIndexValid(buffer, 1) == false);
		  REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 5) == false);
		  REQUIRE(xen::isIndexValid(buffer, 6) == false);
		  CHECK(buffer[2] == 400);
		  CHECK(buffer[3] == 200);
		  CHECK(buffer[4] == 300);
	  }

	  SECTION("Remove Before Wrap"){
		  xen::removeUnordered(buffer, 3);

		  CHECK  (xen::size(buffer) == 3);
		  REQUIRE(xen::isIndexValid(buffer, 1) == false);
		  REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 5) == false);
		  REQUIRE(xen::isIndexValid(buffer, 6) == false);
		  CHECK(buffer[2] == 100);
		  CHECK(buffer[3] == 400);
		  CHECK(buffer[4] == 300);
	  }

	   SECTION("Remove After Wrap"){
		  xen::removeUnordered(buffer, 4);

		  CHECK  (xen::size(buffer) == 3);
		  REQUIRE(xen::isIndexValid(buffer, 1) == false);
		  REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 5) == false);
		  REQUIRE(xen::isIndexValid(buffer, 6) == false);
		  CHECK(buffer[2] == 100);
		  CHECK(buffer[3] == 200);
		  CHECK(buffer[4] == 400);
	  }

	  SECTION("Remove Last"){
		  xen::removeUnordered(buffer, 5);

		  CHECK  (xen::size(buffer) == 3);
		  REQUIRE(xen::isIndexValid(buffer, 1) == false);
		  REQUIRE(xen::isIndexValid(buffer, 2) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 3) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 4) ==  true);
		  REQUIRE(xen::isIndexValid(buffer, 5) == false);
		  REQUIRE(xen::isIndexValid(buffer, 6) == false);
		  CHECK(buffer[2] == 100);
		  CHECK(buffer[3] == 200);
		  CHECK(buffer[4] == 300);
	  }
	}
}


TEST_CASE("RingBufferIterator", "[core][RingBuffer]"){
	xen::RingBuffer<int> buffer = {0};
	int buffer_memory[4];
	buffer.capacity = XenArrayLength(buffer_memory);
	buffer.elements = buffer_memory;

	REQUIRE(xen::size    (buffer) == 0);
	REQUIRE(xen::capacity(buffer) == 4);

	xen::pushBack(buffer,  5);
	xen::pushBack(buffer, 10);

	SECTION("No wrap"){
		auto it_front = xen::iterateFront(buffer);
		auto it_back  = xen::iterateBack (buffer);

		REQUIRE(it_back  - it_front ==  1);
		REQUIRE(it_front - it_back  == -1);

		REQUIRE((bool)it_front[-1] == false);
		REQUIRE((bool)it_front[ 0] ==  true);
		CHECK  (     *it_front[ 0] ==     5);
		REQUIRE((bool)it_front[ 1] ==  true);
		CHECK  (     *it_front[ 1] ==    10);
		REQUIRE((bool)it_front[ 2] == false);

		REQUIRE((bool)it_back[-2] == false);
		REQUIRE((bool)it_back[-1] ==  true);
		CHECK  (     *it_back[-1] ==     5);
		REQUIRE((bool)it_back[ 0] ==  true);
		CHECK  (     *it_back[ 0] ==    10);
		REQUIRE((bool)it_back[ 1] == false);

		CHECK(xen::popFront(buffer) == 5);

		REQUIRE((bool)it_front[-1] == false);
		REQUIRE((bool)it_front[ 0] == false);
		REQUIRE((bool)it_front[ 1] ==  true);
		CHECK  (     *it_front[ 1] ==    10);
		REQUIRE((bool)it_front[ 2] == false);

		REQUIRE((bool)it_back[-2] == false);
		REQUIRE((bool)it_back[-1] == false);
		REQUIRE((bool)it_back[ 0] ==  true);
		CHECK  (     *it_back[ 0] ==    10);
		REQUIRE((bool)it_back[ 1] == false);

		++it_front;

		REQUIRE(it_front == it_back);
		REQUIRE(it_back  - it_front == 0);
		REQUIRE(it_front - it_back  == 0);
		REQUIRE((bool)it_front[-2] == false);
		REQUIRE((bool)it_front[-1] == false);
		REQUIRE((bool)it_front[ 0] ==  true);
		CHECK  (     *it_front[ 0] ==    10);
		REQUIRE((bool)it_front[ 1] == false);
		REQUIRE((bool)it_front[ 2] == false);

		REQUIRE((bool)it_back[-2] == false);
		REQUIRE((bool)it_back[-1] == false);
		REQUIRE((bool)it_back[ 0] ==  true);
		CHECK  (     *it_back[ 0] ==    10);
		REQUIRE((bool)it_back[ 1] == false);
		REQUIRE((bool)it_back[ 2] == false);
	}
}


TEST_CASE("RingBuffer Regression Tests", "[core][RingBuffer]"){
	SECTION("Capacity 8, front 3, empty, push 4", "[core][RingBuffer]"){
		char buffer_memory[8];
		xen::RingBuffer<char> buffer;
		buffer.elements    = buffer_memory;
		buffer.capacity    = 8;
		buffer.front       = 3;
		buffer.size        = 0;
		buffer.first_index = 10;

		xen::pushBack(buffer, 'a');
		xen::pushBack(buffer, 'e');
		xen::pushBack(buffer, 'i');
		xen::pushBack(buffer, 'o');

		REQUIRE(xen::size        (buffer)       ==  4);
		REQUIRE(xen::capacity    (buffer)       ==  8);
		REQUIRE(xen::iterateFront(buffer).index == 10);

		REQUIRE(xen::isIndexValid(buffer,  9) == false);
		REQUIRE(xen::isIndexValid(buffer, 10) ==  true);
		CHECK  (                  buffer [10] ==    'a');
		REQUIRE(xen::isIndexValid(buffer, 11) ==  true);
		CHECK  (                  buffer [11] ==    'e');
		REQUIRE(xen::isIndexValid(buffer, 12) ==  true);
		CHECK  (                  buffer [12] ==    'i');
		REQUIRE(xen::isIndexValid(buffer, 13) ==  true);
		CHECK  (                  buffer [13] ==    'o');
		REQUIRE(xen::isIndexValid(buffer, 14) == false);

		CHECK(*(xen::iterateFront(buffer)   ) == 'a');
		CHECK(*(xen::iterateFront(buffer)[0]) == 'a');
		CHECK(*(xen::iterateFront(buffer)[1]) == 'e');
		CHECK(*(xen::iterateFront(buffer)[2]) == 'i');
		CHECK(*(xen::iterateFront(buffer)[3]) == 'o');

		CHECK(xen::peakFront(buffer) == 'a');
		CHECK(xen::peakBack (buffer) == 'o');

		CHECK(xen::popFront(buffer) == 'a');
		CHECK(xen::popFront(buffer) == 'e');
		CHECK(xen::popFront(buffer) == 'i');
		CHECK(xen::popFront(buffer) == 'o');

	  CHECK(xen::size        (buffer)       ==     0);
		CHECK(xen::capacity    (buffer)       ==     8);
		CHECK(xen::iterateFront(buffer).index ==    14);
	}
}

// :TODO: test that asserts happen correctly when T_ASSERT_ON_OVERFLOW is set
// (would need to have some way of unit test intercepting a XenAssert)
