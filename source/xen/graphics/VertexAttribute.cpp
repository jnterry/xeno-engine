////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains render API independent implementation of Mesh
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_VERTEXATTRIBUTE_CPP
#define XEN_GRAPHICS_VERTEXATTRIBUTE_CPP

#include <xen/core/intrinsics.hpp>
#include <xen/graphics/VertexAttribute.hpp>

namespace xen {
	u32 getVertexAttributeSize(VertexAttribute::Type type){
		u32 result = 0;

		switch(type & VertexAttribute::_TypeMask){
		case VertexAttribute::_TypeFloat  : result = sizeof(float ); break;
		case VertexAttribute::_TypeDouble : result = sizeof(double); break;
		default:
			XenInvalidCodePath("Unhandled type in getVertexAttributeSize");
			break;
		}

		result *= type & VertexAttribute::_ComponentCountMask;

		return result;
	}
}

#endif
