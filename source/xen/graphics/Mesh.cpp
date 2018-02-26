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
#include <xen/core/memory/utilities.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/graphics/Mesh.hpp>

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


	MeshData* createEmptyMeshData(ArenaLinear& arena, const VertexSpec& spec){
		xen::MemoryTransaction transaction(arena);

		xen::MeshData* result = xen::reserveType<xen::MeshData>(arena);

		if(!xen::isValid(arena)){ return nullptr; }

		result->attrib_count = spec.size;
		result->attrib_types = xen::reserveTypeArray<xen::VertexAttribute::Type>(arena, spec.size);
		result->attrib_data  = xen::reserveTypeArray<void*                     >(arena, spec.size);

		if(!xen::isValid(arena)){ return nullptr; }

		xen::copyArray<xen::VertexAttribute::Type>(&spec[0], result->attrib_types, spec.size);
		xen::clearBytes(result->attrib_data, spec.size * sizeof(void*));

		transaction.commit();
		return result;
	}

	bool loadMeshObjFile(MeshData* result, ArenaLinear& arena, const char* path){
		return false;
	}
}

#endif
