#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>

// ---------------------------------------------------------------------------
// Define room coordinates
Vec3r A_room = { 555_r,   0_r,   0_r };
Vec3r B_room = {   0_r,   0_r,   0_r };
Vec3r C_room = { 555_r,   0_r, 555_r };
Vec3r D_room = {   0_r,   0_r, 555_r };

Vec3r E_room = { 555_r, 555_r,   0_r };
Vec3r F_room = {   0_r, 555_r,   0_r };
Vec3r G_room = { 555_r, 555_r, 555_r };
Vec3r H_room = {   0_r, 555_r, 555_r };

// ---------------------------------------------------------------------------
// Define short block coordinates
Vec3r A_short = { 265_r,   0_r, 441_r };
Vec3r B_short = { 425_r,   0_r, 490_r };
Vec3r C_short = { 315_r,   0_r, 283_r };
Vec3r D_short = {  82_r,   0_r, 330_r };

Vec3r E_short = { 265_r, 165_r, 441_r };
Vec3r F_short = { 425_r, 165_r, 490_r };
Vec3r G_short = { 315_r, 165_r, 283_r };
Vec3r H_short = { 473_r, 165_r, 330_r };

// ---------------------------------------------------------------------------
// Define tall block coordinates
Vec3r A_tall = { 132_r,   0_r, 308_r };
Vec3r B_tall = { 290_r,   0_r, 259_r };
Vec3r C_tall = {  83_r,   0_r, 149_r };
Vec3r D_tall = { 241_r,   0_r,  99_r };

Vec3r E_tall = { 132_r, 330_r, 308_r };
Vec3r F_tall = { 290_r, 330_r, 259_r };
Vec3r G_tall = {  83_r, 330_r, 149_r };
Vec3r H_tall = { 241_r, 330_r,  99_r };

// ---------------------------------------------------------------------------
// Define colors
Vec3r red    = { 0.75_r, 0.15_r, 0.15_r };
Vec3r yellow = { 0.75_r, 0.75_r, 0.15_r };
Vec3r green  = { 0.15_r, 0.75_r, 0.15_r };
Vec3r cyan   = { 0.15_r, 0.75_r, 0.75_r };
Vec3r blue   = { 0.15_r, 0.15_r, 0.75_r };
Vec3r purple = { 0.75_r, 0.15_r, 0.75_r };
Vec3r white  = { 0.75_r, 0.75_r, 0.75_r };

Vec3r test_model_geometry_array[] = {
	// ---------------------------------------------------------------------------
	// room coordinates
	// Floor
	C_room, B_room, A_room,
	C_room, D_room, B_room,
	// Left Wall
	A_room, E_room, C_room,
	C_room, E_room, G_room,
	// Right Wall
	F_room, B_room, D_room,
	H_room, F_room, D_room,
	// Ceiling
	E_room, F_room, G_room,
	H_room, F_room, D_room,
	// Back Wall
	G_room, D_room, C_room,
	G_room, H_room, D_room,
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

Vec3r test_model_color_array[] = {
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

Vec3r *test_model_geometry = test_model_geometry_array;
Vec3r *test_model_color = test_model_color_array;

int test_model_num_vertices = XenArrayLength(test_model_geometry_array);
