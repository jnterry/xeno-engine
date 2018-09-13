////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen StringBuffer type and helper functions
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/StringBuffer.hpp>
#include <cstring>
#include <catch.hpp>

#define BUFFER_SIZE 64
char buffer_data[128];

void clearBuffer(){
	for(u64 i = 0; i < BUFFER_SIZE; ++i){
		buffer_data[i] = '-';
	}
}

#if 0
void printBuffer(){
	for(u32 i = 0; i < BUFFER_SIZE; ++i){
		printf("|%c", buffer_data[i] == '\0' ? '_' : buffer_data[i]);
	}

	printf("|\n\n");
}
#else
void printBuffer(){}
#endif

TEST_CASE("StringBuffer construction", "[core][string]"){

	{
		clearBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "test", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer, "test") == 0);
		CHECK(buffer.buffer_start == buffer.start);
	}

	{
		clearBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "Hello World!", 10);

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer, "Hello World!") == 0);
		CHECK(&buffer.buffer_start[10] == buffer.start);
	}

}

TEST_CASE("StringBuffer prepend", "[core][string]"){
	clearBuffer();

	SECTION("String at start of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "World!", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "World!") == 0);
		CHECK(strcmp(buffer.start,        "World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "World!") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "Hello ");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "Hello World!") != 0);
	}

	SECTION("Insufficient space at start of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "World!", 3);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "World!") == 0);
		CHECK(strcmp(buffer.start, "World!") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "Hello ");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "Hello World!") != 0);
	}

	SECTION("Just enough space at start of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "World!", 6);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "World!") == 0);
		CHECK(strcmp(buffer.start,        "World!") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "Hello ");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "Hello World!") == 0);
	}

	SECTION("Plenty of space at start of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "World!", 1000);

	  REQUIRE(buffer.buffer_start == buffer_data);
	  CHECK(buffer.buffer_end[ 0] == '\0');
	  CHECK(buffer.buffer_end[-1] == '!');
		CHECK(strcmp(buffer,              "World!") == 0);
		CHECK(strcmp(buffer.start,        "World!") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "Hello ");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "Hello World!") != 0);
	}

	SECTION("Double prepend"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "World!", 1000);

	  REQUIRE(buffer.buffer_start == buffer_data);
	  CHECK(buffer.buffer_end[ 0] == '\0');
	  CHECK(buffer.buffer_end[-1] == '!');
		CHECK(strcmp(buffer,              "World!") == 0);
		CHECK(strcmp(buffer.start,        "World!") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "Hello ");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "Hello World!") != 0);

		xen::stringPrepend(buffer, "123");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123Hello World!") == 0);
		CHECK(strcmp(buffer.start,        "123Hello World!") == 0);
		CHECK(strcmp(buffer.buffer_start, "123Hello World!") != 0);
	}

	SECTION("Tight buffer - string at start"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, 5, "cd", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "cd") == 0);
		CHECK(strcmp(buffer.start,        "cd") == 0);
		CHECK(strcmp(buffer.buffer_start, "cd") == 0);

		printBuffer();

		xen::stringPrepend(buffer, "ab");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "abcd") == 0);
		CHECK(strcmp(buffer.start,        "abcd") == 0);
		CHECK(strcmp(buffer.buffer_start, "abcd") == 0);
	}

	SECTION("Tight buffer - string at end"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, 5, "cd", 1000);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "cd") == 0);
		CHECK(strcmp(buffer.start,        "cd") == 0);
		CHECK(buffer.start != buffer.buffer_start);

		printBuffer();

		xen::stringPrepend(buffer, "ab");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "abcd") == 0);
		CHECK(strcmp(buffer.start,        "abcd") == 0);
		CHECK(strcmp(buffer.buffer_start, "abcd") == 0);
	}
}




TEST_CASE("StringBuffer append", "[core][string]"){
	clearBuffer();

	SECTION("String at start of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "123", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123") == 0);
		CHECK(strcmp(buffer.start,        "123") == 0);
		CHECK(strcmp(buffer.buffer_start, "123") == 0);

		printBuffer();

		xen::stringAppend(buffer, "abc");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abc") == 0);
		CHECK(strcmp(buffer.start,        "123abc") == 0);
		CHECK(strcmp(buffer.buffer_start, "123abc") == 0);
	}

	SECTION("Insufficient space at end of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "123", BUFFER_SIZE-5);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,       "123") == 0);
		CHECK(strcmp(buffer.start, "123") == 0);

		printBuffer();

		xen::stringAppend(buffer, "abc");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abc") == 0);
		CHECK(strcmp(buffer.start,        "123abc") == 0);
		CHECK(buffer.buffer_end[0] != '\0');
	}

	SECTION("Just enough space at end of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "123", BUFFER_SIZE-7);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,       "123") == 0);
		CHECK(strcmp(buffer.start, "123") == 0);

		printBuffer();

		xen::stringAppend(buffer, "abc");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abc") == 0);
		CHECK(strcmp(buffer.start,        "123abc") == 0);
		CHECK(buffer.buffer_end[ 0] == '\0');
		CHECK(buffer.buffer_end[-1] == 'c');
	}

	SECTION("Plenty of space at end of buffer"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "123", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
	  CHECK(strcmp(buffer.buffer_start, "123") == 0);
		CHECK(strcmp(buffer,              "123") == 0);
		CHECK(strcmp(buffer.start,        "123") == 0);

		printBuffer();

		xen::stringAppend(buffer, "abctestthing");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abctestthing") == 0);
		CHECK(strcmp(buffer.start,        "123abctestthing") == 0);
		CHECK(strcmp(buffer.buffer_start, "123abctestthing") == 0);
	}

	SECTION("Double append"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, BUFFER_SIZE, "123");

	  REQUIRE(buffer.buffer_start == buffer_data);
	  CHECK(strcmp(buffer.buffer_start, "123") == 0);
		CHECK(strcmp(buffer,              "123") == 0);
		CHECK(strcmp(buffer.start,        "123") == 0);

		printBuffer();

		xen::stringAppend(buffer, "abc");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abc") == 0);
		CHECK(strcmp(buffer.start,        "123abc") == 0);
		CHECK(strcmp(buffer.buffer_start, "123abc") == 0);

		xen::stringAppend(buffer, "987");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "123abc987") == 0);
		CHECK(strcmp(buffer.start,        "123abc987") == 0);
		CHECK(strcmp(buffer.buffer_start, "123abc987") == 0);
	}


	SECTION("Tight buffer - string at start"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, 5, "ab", 0);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "ab") == 0);
		CHECK(strcmp(buffer.start,        "ab") == 0);
		CHECK(strcmp(buffer.buffer_start, "ab") == 0);

		printBuffer();

		xen::stringAppend(buffer, "xy");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "abxy") == 0);
		CHECK(strcmp(buffer.start,        "abxy") == 0);
		CHECK(strcmp(buffer.buffer_start, "abxy") == 0);
	}

	SECTION("Tight buffer - string at end"){
		printBuffer();
		xen::StringBuffer buffer(buffer_data, 5, "cd", 1000);

	  REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "cd") == 0);
		CHECK(strcmp(buffer.start,        "cd") == 0);
		CHECK(buffer.start != buffer.buffer_start);

		printBuffer();

		xen::stringAppend(buffer, "ab");

		printBuffer();

		REQUIRE(buffer.buffer_start == buffer_data);
		CHECK(strcmp(buffer,              "cdab") == 0);
		CHECK(strcmp(buffer.start,        "cdab") == 0);
		CHECK(strcmp(buffer.buffer_start, "cdab") == 0);
		}
}



TEST_CASE("StringBuffer mixed prepend and append", "[core][string]"){
	for(u64 i = 0; i < 14; ++i){
		clearBuffer();
		xen::StringBuffer buffer(buffer_data, 14, "123", i);
		printBuffer();
		xen::stringAppend(buffer, "xyz");
		xen::stringPrepend(buffer, "3");
		xen::stringPrepend(buffer, "2");
		xen::stringPrepend(buffer, "1");
		xen::stringAppend (buffer, "!");
		xen::stringAppend (buffer, "@@@");
		printBuffer();
		CHECK(strcmp(buffer, "123123xyz!@@@") == 0);
		CHECK(buffer.buffer_start == buffer_data);
	}
}
