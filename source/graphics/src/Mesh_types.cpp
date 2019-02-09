////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of MeshType static instances
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESHTYPES_CPP
#define XEN_GRAPHICS_MESHTYPES_CPP

#include <xen/graphics/Mesh_types.hpp>

const xen::VertexAttribute xen::VertexAttribute::Position3r = {
	Aspect::Position, Type::Real  | 3
};
const xen::VertexAttribute xen::VertexAttribute::Normal3r   = {
	Aspect::Normal,   Type::Real  | 3
};
const xen::VertexAttribute xen::VertexAttribute::Color3f    = {
	Aspect::Color,    Type::Float | 3
};
const xen::VertexAttribute xen::VertexAttribute::Color4b    = {
	Aspect::Color,    Type::Byte  | 4
};
const xen::VertexAttribute xen::VertexAttribute::TexCoord2f = {
	Aspect::TexCoord, Type::Float | 2
};

#endif
