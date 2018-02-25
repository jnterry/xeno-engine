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
}

#endif
