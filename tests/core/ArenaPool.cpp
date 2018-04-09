////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains unit tests for xen::ArenaPool
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/Allocator.hpp>
#include <catch.hpp>

xen::Allocator* allocator = new xen::AllocatorMalloc();

struct Thing {
	int x, y;
};

TEST_CASE("ArenaPool", "[core][memory][ArenaPool]"){
	xen::ArenaPool<Thing> pool = xen::createArenaPool<Thing>(allocator, 4);

	REQUIRE(pool.slots != nullptr);
	REQUIRE(pool.capacity   == 4);
	REQUIRE(pool.slots_used == 0);

	SECTION("Single allocation"){
		Thing* t = xen::reserveType<Thing>(pool);

		t->x = 10; // write to thing to make sure this doesn't corrupt the pool
		t->y = 20;

		CHECK(pool.capacity   ==  4);
		CHECK(pool.slots_used ==  1);
		CHECK(t->x            == 10);
		CHECK(t->y            == 20);
	}

	SECTION("Too many allocations"){
		Thing* ta = xen::reserveType<Thing>(pool);
		Thing* tb = xen::reserveType<Thing>(pool);
		Thing* tc = xen::reserveType<Thing>(pool);
		Thing* td = xen::reserveType<Thing>(pool);
		Thing* te = xen::reserveType<Thing>(pool);

		REQUIRE(te == nullptr);

		ta->x = 10; // write to thing to make sure this doesn't corrupt the pool
		ta->y = 11;
		tb->x = 12; // Also check that each instance is really separate and doesn't
		tb->y = 13; // overwrite memory of another
		tc->x = 14;
		tc->y = 15;
		td->x = 16;
		td->y = 17;

		CHECK(pool.capacity   ==  4);
		CHECK(pool.slots_used ==  4);
		CHECK(ta->x           == 10);
		CHECK(ta->y           == 11);
		CHECK(tb->x           == 12);
		CHECK(tb->y           == 13);
		CHECK(tc->x           == 14);
		CHECK(tc->y           == 15);
		CHECK(td->x           == 16);
		CHECK(td->y           == 17);

		xen::freeType<Thing>(pool, ta);

		CHECK(pool.capacity   ==  4);
		CHECK(pool.slots_used ==  3);

		Thing* tf = xen::reserveType<Thing>(pool);
		tf->x = 18;
		tf->y = 19;

		CHECK(pool.capacity   ==  4);
		CHECK(pool.slots_used ==  4);
		CHECK(tf->x           == 18);
		CHECK(tf->y           == 19);
		CHECK(tb->x           == 12);
		CHECK(tb->y           == 13);
		CHECK(tc->x           == 14);
		CHECK(tc->y           == 15);
		CHECK(td->x           == 16);
		CHECK(td->y           == 17);
	}
}
