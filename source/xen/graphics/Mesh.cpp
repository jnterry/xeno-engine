////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API independent implementation of Mesh
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_MESH_CPP
#define XEN_GRAPHICS_MESH_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/graphics/Mesh.hpp>

namespace xen {
	u32 getVertexAttributeTypeSize(VertexAttributeType::Type type){
		u32 result = 0;

		switch(type & VertexAttributeType::_TypeMask){
		case VertexAttributeType::_TypeFloat  : result = sizeof(float ); break;
		case VertexAttributeType::_TypeDouble : result = sizeof(double); break;
		default:
			XenInvalidCodePath("Unhandled type in getVertexAttributeTypeSize");
			break;
		}

		result *= type & VertexAttributeType::_ComponentCountMask;

		return result;
	}

	VertexAttributeSource getDefaultVertexAttributeSource(xen::VertexAttributeType::Type type) {
		xen::VertexAttributeSource source = {0};

		switch(type){
		case xen::VertexAttributeType::Position3r:
			source.vec3r = {0,0,0};
			break;
		case xen::VertexAttributeType::Normal3r:
			source.vec3r = {1,0,0};
			break;
		case xen::VertexAttributeType::Color3f:
			source.color3f = xen::Color::WHITE4f.rgb;
			break;
		default:
			XenInvalidCodePath("Unhandled vertex attribute type while setting default source");
			break;
		}

		return source;
	}
}

#endif
