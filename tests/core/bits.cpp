////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xeno engines helper types and functions for
/// bit manipulation
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

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

TEST_CASE("BitField8", "[core][bits]"){

	// 147
	// bit indices : 7 6 5 4 3 2 1 0
	// binary      : 1 0 0 1 0 0 1 1
	xen::BitField<u8, 8> field_a = 147;

	REQUIRE(field_a[0] ==  true);
	REQUIRE(field_a[1] ==  true);
	REQUIRE(field_a[2] == false);
	REQUIRE(field_a[3] == false);
	REQUIRE(field_a[4] ==  true);
	REQUIRE(field_a[5] == false);
	REQUIRE(field_a[6] == false);
	REQUIRE(field_a[7] ==  true);

	SECTION("Bitwise Not"){
		auto result = ~field_a;
	  CHECK(result[0] == false);
	  CHECK(result[1] == false);
	  CHECK(result[2] ==  true);
	  CHECK(result[3] ==  true);
	  CHECK(result[4] == false);
	  CHECK(result[5] ==  true);
	  CHECK(result[6] ==  true);
	  CHECK(result[7] == false);
	}

	// 66
	// bit indices : 7 6 5 4 3 2 1 0
	// binary      : 0 1 0 0 0 0 1 0
	xen::BitField<u08, 8> field_b(66);
	REQUIRE(field_b[0] == false);
	REQUIRE(field_b[1] ==  true);
	REQUIRE(field_b[2] == false);
	REQUIRE(field_b[3] == false);
	REQUIRE(field_b[4] == false);
	REQUIRE(field_b[5] == false);
	REQUIRE(field_b[6] ==  true);
	REQUIRE(field_b[7] == false);

	// bit indices : 7 6 5 4 3 2 1 0
	// field_a     : 1 0 0 1 0 0 1 1
	// field_b     : 0 1 0 0 0 0 1 0

	SECTION("Bitwise Or"){
		auto result = field_a | field_b;
	  CHECK(result[0] ==  true);
	  CHECK(result[1] ==  true);
	  CHECK(result[2] == false);
	  CHECK(result[3] == false);
	  CHECK(result[4] ==  true);
	  CHECK(result[5] == false);
	  CHECK(result[6] ==  true);
	  CHECK(result[7] ==  true);
	}

	SECTION("Bitwise EqOr"){
	  field_a |= field_b;
	  CHECK(field_a[0] ==  true);
	  CHECK(field_a[1] ==  true);
	  CHECK(field_a[2] == false);
	  CHECK(field_a[3] == false);
	  CHECK(field_a[4] ==  true);
	  CHECK(field_a[5] == false);
	  CHECK(field_a[6] ==  true);
	  CHECK(field_a[7] ==  true);
	}

	SECTION("Bitwise And"){
		auto result = field_a & field_b;
	  CHECK(result[0] == false);
	  CHECK(result[1] ==  true);
	  CHECK(result[2] == false);
	  CHECK(result[3] == false);
	  CHECK(result[4] == false);
	  CHECK(result[5] == false);
	  CHECK(result[6] == false);
	  CHECK(result[7] == false);
	}

	SECTION("Bitwise EqAnd"){
	  field_a &= field_b;
	  CHECK(field_a[0] == false);
	  CHECK(field_a[1] ==  true);
	  CHECK(field_a[2] == false);
	  CHECK(field_a[3] == false);
	  CHECK(field_a[4] == false);
	  CHECK(field_a[5] == false);
	  CHECK(field_a[6] == false);
	  CHECK(field_a[7] == false);
	}

	SECTION("Bitwise Xor"){
		auto result = field_a ^ field_b;
	  CHECK(result[0] ==  true);
	  CHECK(result[1] == false);
	  CHECK(result[2] == false);
	  CHECK(result[3] == false);
	  CHECK(result[4] ==  true);
	  CHECK(result[5] == false);
	  CHECK(result[6] ==  true);
	  CHECK(result[7] ==  true);
	}

	SECTION("Bitwise EqXor"){
	  field_a ^= field_b;
	  CHECK(field_a[0] ==  true);
	  CHECK(field_a[1] == false);
	  CHECK(field_a[2] == false);
	  CHECK(field_a[3] == false);
	  CHECK(field_a[4] ==  true);
	  CHECK(field_a[5] == false);
	  CHECK(field_a[6] ==  true);
	  CHECK(field_a[7] ==  true);
	}

}
