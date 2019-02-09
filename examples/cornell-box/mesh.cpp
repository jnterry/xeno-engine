#include <xen/core/intrinsics.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/graphics/RenderCommand3d.hpp>
#include <xen/graphics/TestMeshes.hpp>

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

static_assert(XenArrayLength(positions_walls) == XenArrayLength(colors_walls),
              "Expected equal number of colors and positions"
             );
static_assert(XenArrayLength(positions_walls) == XenArrayLength(normals_walls),
              "Expected equal number of colors and positions"
             );

void* _walls_arrays[4] = {
	positions_walls, normals_walls, colors_walls, nullptr
};

const xen::MeshData _MeshData_CornellBoxWalls = {
	XenArrayLength(positions_walls),
	xen::PrimitiveType::Triangles,
	xen::TestMeshVertexSpec,
	{ { 0.0_r, 0.0_r, 0.0_r }, { 1.0_r, 1.0_r, 1.0_r } },
	_walls_arrays
};


const xen::MeshData* MeshData_CornellBoxWalls    = &_MeshData_CornellBoxWalls;
