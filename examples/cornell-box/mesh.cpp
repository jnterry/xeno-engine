#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/RenderCommand3d.hpp>

#include "mesh.hpp"

// ---------------------------------------------------------------------------
// Define room coordinates
Vec3r A_room = { 1_r,   0_r, 0_r };
Vec3r B_room = { 0_r,   0_r, 0_r };
Vec3r C_room = { 1_r,   0_r, 1_r };
Vec3r D_room = { 0_r,   0_r, 1_r };

Vec3r E_room = { 1_r, 1_r, 0_r };
Vec3r F_room = { 0_r, 1_r, 0_r };
Vec3r G_room = { 1_r, 1_r, 1_r };
Vec3r H_room = { 0_r, 1_r, 1_r };

// ---------------------------------------------------------------------------
// Define short block coordinates
Vec3r A_short = {290_r,   0_r, 114_r};
Vec3r B_short = {130_r,   0_r,  65_r};
Vec3r C_short = {240_r,   0_r, 272_r};
Vec3r D_short = { 82_r,   0_r, 225_r};

Vec3r E_short = {290_r, 165_r, 114_r};
Vec3r F_short = {130_r, 165_r,  65_r};
Vec3r G_short = {240_r, 165_r, 272_r};
Vec3r H_short = { 82_r, 165_r, 225_r};

// ---------------------------------------------------------------------------
// Define tall block coordinates
Vec3r A_tall = {423_r,   0_r, 247_r};
Vec3r B_tall = {265_r,   0_r, 296_r};
Vec3r C_tall = {472_r,   0_r, 406_r};
Vec3r D_tall = {314_r,   0_r, 456_r};

Vec3r E_tall = {423_r, 330_r, 247_r};
Vec3r F_tall = {265_r, 330_r, 296_r};
Vec3r G_tall = {472_r, 330_r, 406_r};
Vec3r H_tall = {314_r, 330_r, 456_r};

Vec3r positions_walls[] = {
	// ---------------------------------------------------------------------------
	// room coordinates
	// Floor
	C_room, A_room, B_room,
	C_room, B_room, D_room,
	// Left Wall
	A_room, C_room, E_room,
	C_room, G_room, E_room,
	// Right Wall
	F_room, D_room, B_room,
	H_room, D_room, F_room,
	// Ceiling
	E_room, G_room, F_room,
	F_room, G_room, H_room,
	// Back Wall
	G_room, C_room, D_room,
	G_room, D_room, H_room,
};

Vec3r normals_walls[] = {
	// Floor
	Vec3r::UnitY, Vec3r::UnitY, Vec3r::UnitY,
	Vec3r::UnitY, Vec3r::UnitY, Vec3r::UnitY,

	// Left Wall
  Vec3r::UnitX, Vec3r::UnitX, Vec3r::UnitX,
	Vec3r::UnitX, Vec3r::UnitX, Vec3r::UnitX,

	// Right Wall
	-Vec3r::UnitX, -Vec3r::UnitX, -Vec3r::UnitX,
	-Vec3r::UnitX, -Vec3r::UnitX, -Vec3r::UnitX,

	// Ceiling
  -Vec3r::UnitY, -Vec3r::UnitY, -Vec3r::UnitY,
	-Vec3r::UnitY, -Vec3r::UnitY, -Vec3r::UnitY,

	// Back Wall
	Vec3r::UnitZ, Vec3r::UnitZ, Vec3r::UnitZ,
	Vec3r::UnitZ, Vec3r::UnitZ, Vec3r::UnitZ,
};

Vec3r positions_interior[] = {
	// ---------------------------------------------------------------------------
	// Short block coordinates
	// Front
	E_short, B_short, A_short,
	E_short, F_short, B_short,
	// Right
	F_short, D_short, B_short,
	F_short, H_short, D_short,
	// Back
	H_short, C_short, D_short,
	H_short, G_short, C_short,
	// LEFT
	G_short, E_short, C_short,
	E_short, A_short, C_short,
	// TOP
	G_short, F_short, E_short,
	G_short, H_short, F_short,
	// ---------------------------------------------------------------------------
	// Tall block coordinates
	// Front
	E_tall, B_tall, A_tall,
	E_tall, F_tall, B_tall,
	// Front
	F_tall, D_tall, B_tall,
	F_tall, H_tall, D_tall,
	// BACK
	H_tall, C_tall, D_tall,
	H_tall, G_tall, C_tall,
	// LEFT
	G_tall, E_tall, C_tall,
	E_tall, A_tall, C_tall,
	// TOP
	G_tall, F_tall, E_tall,
	G_tall, H_tall, F_tall,
};


// ---------------------------------------------------------------------------
// Define colors
xen::Color red    = { 191,  38,  38, 255 };
xen::Color yellow = { 191, 191,  38, 255 };
xen::Color green  = {  38, 191,  38, 255 };
xen::Color cyan   = {  38, 191, 191, 255 };
xen::Color blue   = {  38,  38, 191, 255 };
xen::Color purple = { 191,  38, 191, 255 };
xen::Color white  = { 191, 191, 191, 255 };

xen::Color colors_walls[] = {
	// ---------------------------------------------------------------------------
	// Room colors
	// Floor
	green, green, green,
	green, green, green,
	// Left Wall
	purple, purple, purple,
	purple, purple, purple,
	// Right Wall
	yellow, yellow, yellow,
	yellow, yellow, yellow,
	// Ceiling
	cyan, cyan, cyan,
	cyan, cyan, cyan,
	// Back wall
	white, white, white,
	white, white, white,
};

xen::Color colors_interior[] = {
	// ---------------------------------------------------------------------------
	// Short block colors
	// Front
	red, red, red,
	red, red, red,
	// Right
	red, red, red,
	red, red, red,
	// Back
	red, red, red,
	red, red, red,
	// Left
	red, red, red,
	red, red, red,
	// Top
	red, red, red,
	red, red, red,
	// ---------------------------------------------------------------------------
	// Tall block colors
	// Front
	blue, blue, blue,
	blue, blue, blue,
	// Right
	blue, blue, blue,
	blue, blue, blue,
	// Back
	blue, blue, blue,
	blue, blue, blue,
	// Left
	blue, blue, blue,
	blue, blue, blue,
	// Top
	blue, blue, blue,
	blue, blue, blue,

};

static_assert(XenArrayLength(positions_walls) == XenArrayLength(colors_walls),
              "Expected equal number of colors and positions"
             );
static_assert(XenArrayLength(positions_walls) == XenArrayLength(normals_walls),
              "Expected equal number of colors and positions"
             );

static_assert(XenArrayLength(positions_interior) == XenArrayLength(colors_interior),
              "Expected equal number of colors and positions"
             );

const xen::MeshAttribArrays MeshGeometry_CornellBoxWalls = {
	XenArrayLength(positions_walls),
  positions_walls,
	normals_walls,
	colors_walls,
	nullptr
};

const xen::MeshAttribArrays MeshGeometry_CornellBoxInterior = {
	XenArrayLength(positions_interior),
  positions_interior,
	nullptr, // normals
	colors_interior,
	nullptr
};
