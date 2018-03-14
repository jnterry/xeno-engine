#include <xen/core/bits.hpp>
#include <catch.hpp>

TEST_CASE("Const Bit Reference", "[core][bits]"){
	//   bit index : 6 5 4 3 2 1 0
	const int val = 36; // 0 1 0 0 1 0 0

	const xen::BitReference<int> bit0 = xen::makeBitReference(&val, 0);
	const xen::BitReference<int> bit1 = xen::makeBitReference(&val, 1);
	const xen::BitReference<int> bit2 = xen::makeBitReference(&val, 2);
	const xen::BitReference<int> bit3 = xen::makeBitReference(&val, 3);
	const xen::BitReference<int> bit4 = xen::makeBitReference(&val, 4);
	const xen::BitReference<int> bit5 = xen::makeBitReference(&val, 5);
	const xen::BitReference<int> bit6 = xen::makeBitReference(&val, 6);

	CHECK(bit0 == false);
	CHECK(bit1 == false);
	CHECK(bit2 ==  true);
	CHECK(bit3 == false);
	CHECK(bit4 == false);
	CHECK(bit5 ==  true);
	CHECK(bit6 == false);
}

TEST_CASE("Non-const Bit Reference", "[core][bits]"){
  //   bit index : 6 5 4 3 2 1 0
	int val = 42; // 0 1 0 1 0 1 0

  auto bit0 = xen::makeBitReference(&val, 0);
	auto bit1 = xen::makeBitReference(&val, 1);
	auto bit2 = xen::makeBitReference(&val, 2);
	auto bit3 = xen::makeBitReference(&val, 3);
	auto bit4 = xen::makeBitReference(&val, 4);
	auto bit5 = xen::makeBitReference(&val, 5);
	auto bit6 = xen::makeBitReference(&val, 6);

	REQUIRE(bit0 == false);
	REQUIRE(bit1 ==  true);
	REQUIRE(bit2 == false);
	REQUIRE(bit3 ==  true);
	REQUIRE(bit4 == false);
	REQUIRE(bit5 ==  true);
	REQUIRE(bit6 == false);

	SECTION("Set bit to 1"){
		bit0 = 1;
		CHECK(bit0 ==  true);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 == false);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    43);
	}

	SECTION("Set bit to true"){
		bit4 = true;
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 ==  true);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    58);
	}

	SECTION("Set bit"){
		bit6.set();
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 == false);
		CHECK(bit5 ==  true);
		CHECK(bit6 ==  true);
		CHECK(val  ==   106);
	}

	SECTION("Set bit to 0"){
		bit3 = 0;
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 == false);
		CHECK(bit4 == false);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    34);
	}

	SECTION("Set bit to false"){
		bit1 = false;
		CHECK(bit0 == false);
		CHECK(bit1 == false);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 == false);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    40);
	}

	SECTION("Clear bit"){
		bit5.clear();
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 == false);
		CHECK(bit5 == false);
		CHECK(bit6 == false);
		CHECK(val  ==    10);
	}

	SECTION("toogle"){
		bit4.toogle();
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 ==  true);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    58);
	}

	SECTION("Set equal to not self"){
		bit4 = !bit4;
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 ==  true);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    58);
	}

	SECTION("Set equal to not another"){
		bit4 = !bit0;
		CHECK(bit0 == false);
		CHECK(bit1 ==  true);
		CHECK(bit2 == false);
		CHECK(bit3 ==  true);
		CHECK(bit4 ==  true);
		CHECK(bit5 ==  true);
		CHECK(bit6 == false);
		CHECK(val  ==    58);
	}
}
