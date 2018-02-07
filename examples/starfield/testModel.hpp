// Defines a simple test model: The Cornel Box

// ---------------------------------------------------------------------------
// Define room coordinates
Vec3r A_room = { 555,   0,   0};
Vec3r B_room = {   0,   0,   0};
Vec3r C_room = { 555,   0, 555};
Vec3r D_room = {   0,   0, 555};

Vec3r E_room = { 555, 555,   0};
Vec3r F_room = {   0, 555,   0};
Vec3r G_room = { 555, 555, 555};
Vec3r H_room = {   0, 555, 555};

// ---------------------------------------------------------------------------
// Define short block coordinates
Vec3r A_short = { 290,   0, 114};
Vec3r B_short = { 130,   0,  65};
Vec3r C_short = { 240,   0, 272};
Vec3r D_short = {  82,   0, 225};

Vec3r E_short = { 290, 165, 114};
Vec3r F_short = { 130, 165,  65};
Vec3r G_short = { 240, 165, 272};
Vec3r H_short = {  82, 165, 225};

// ---------------------------------------------------------------------------
// Define tall block coordinates
Vec3r A_tall = { 423,   0, 247};
Vec3r B_tall = { 265,   0, 296};
Vec3r C_tall = { 472,   0, 406};
Vec3r D_tall = { 314,   0, 456};

Vec3r E_tall = { 423, 330, 247};
Vec3r F_tall = { 265, 330, 296};
Vec3r G_tall = { 472, 330, 406};
Vec3r H_tall = { 314, 330, 456};

// ---------------------------------------------------------------------------
// Define colors
Vec3r red    = { 0.75f, 0.15f, 0.15f };
Vec3r yellow = { 0.75f, 0.75f, 0.15f };
Vec3r green  = { 0.15f, 0.75f, 0.15f };
Vec3r cyan   = { 0.15f, 0.75f, 0.75f };
Vec3r blue   = { 0.15f, 0.15f, 0.75f };
Vec3r purple = { 0.75f, 0.15f, 0.75f };
Vec3r white  = { 0.75f, 0.75f, 0.75f };

Vec3r test_model_geometry[] = {
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
	E_tall, B_tall, A_tall
	E_tall, F_tall, B_tall
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
}

Vec3r test_model_color[] = {
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

}
