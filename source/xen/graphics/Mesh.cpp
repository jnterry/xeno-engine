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
#include <xen/core/memory/Allocator.hpp>
#include <xen/math/geometry.hpp>
#include <xen/util/File.hpp>
#include <xen/graphics/Mesh.hpp>

#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include <assimp/vector3.h>
#include <assimp/color4.h>

namespace {
	struct VertexAttributeAspects {
		u08 position;
		u08 normal;
		u08 color;
		u08 texcoord;
		static const u08 BAD_INDEX = 255;
	};

	VertexAttributeAspects findVertexAttributeAspectIndicies(xen::VertexAttribute::Type* types,
	                                                         u32 attrib_count){
		VertexAttributeAspects result = {
			VertexAttributeAspects::BAD_INDEX,
			VertexAttributeAspects::BAD_INDEX,
			VertexAttributeAspects::BAD_INDEX,
			VertexAttributeAspects::BAD_INDEX
		};

		for(u32 i = 0; i < attrib_count; ++i){
			switch(types[i] & xen::VertexAttribute::_AspectMask){
			case xen::VertexAttribute::_AspectPosition:
				XenAssert(result.position == VertexAttributeAspects::BAD_INDEX,
				          "We don't support multiple positions per vertex");
				result.position = i;
				break;
			case xen::VertexAttribute::_AspectNormal:
				XenAssert(result.normal == VertexAttributeAspects::BAD_INDEX,
				          "We don't support multiple normals per vertex");
				result.normal = i;
				break;
			case xen::VertexAttribute::_AspectColor:
				// :TODO: multiple color channels
				XenAssert(result.color == VertexAttributeAspects::BAD_INDEX,
				          "We don't support multiple colors per vertex... YET");
				result.color = i;
				break;
			case xen::VertexAttribute::_AspectTexCoord:
				// :TODO: multiple texture channels
				XenAssert(result.texcoord == VertexAttributeAspects::BAD_INDEX,
				          "We don't support multiple tex coords per vertex... YET");
				result.texcoord = i;
				break;
			default:
				XenInvalidCodePath("Missing case");
				break;
			}
		}

		return result;
	}

	//VertexAttributeAspects findVertexAttributeAspectIndicies(xen::VertexSpec& spec){
	//	return findVertexAttributeAspectIndicies(&spec[0], spec.size);
	//}

	template<typename T>
	Vec3<T> aiVector3dToXenVec3(aiVector3D v){
		return Vec3<T> { (T)v.x, (T)v.y, (T)v.z };
	}

	xen::Color4f aiColor4dToXenColor4f(aiColor4D c){
		return xen::Color4f { (float)c.r, (float)c.g, (float)c.b, (float)c.a };
	}

	bool processAssimpMesh(xen::MeshData* md, xen::ArenaLinear& arena, const aiMesh* mesh){
		xen::MemoryTransaction transaction(arena);
		VertexAttributeAspects aspect = findVertexAttributeAspectIndicies(md->attrib_types,
		                                                                  md->attrib_count
		                                                                 );

		XenAssert(mesh->mNumFaces > 0, "Expected mesh to contain some faces");
		XenAssert(mesh->mPrimitiveTypes == aiPrimitiveType_TRIANGLE, "Expected mesh to be triangulated");

		int num_triangles = mesh->mNumFaces;

		xen::Triangle3r* buffer_position = nullptr;
		xen::Triangle3r* buffer_normal   = nullptr;
		xen::Triangle4f* buffer_color    = nullptr;
		xen::Triangle2f* buffer_texcoord = nullptr;

		if(aspect.position != VertexAttributeAspects::BAD_INDEX &&
		   mesh->HasPositions()){
			buffer_position = xen::reserveTypeArray<xen::Triangle3r>(arena, num_triangles);
		}
		if(aspect.normal   != VertexAttributeAspects::BAD_INDEX &&
		   mesh->HasNormals()){
			buffer_normal = xen::reserveTypeArray<xen::Triangle3r>(arena, num_triangles);
		}
		if(aspect.color    != VertexAttributeAspects::BAD_INDEX &&
		   mesh->HasVertexColors(0)){
			buffer_color = xen::reserveTypeArray<xen::Triangle4f>(arena, num_triangles);
		}
		if(aspect.texcoord != VertexAttributeAspects::BAD_INDEX &&
		   mesh->HasTextureCoords(0)){
			buffer_texcoord = xen::reserveTypeArray<xen::Triangle2f>(arena, num_triangles);

			// :TODO: support 1d/3d textures
			XenAssert(mesh->mNumUVComponents[0] == 2, "Expecting 2 UV coordinates per vertex");
		}

		if(!xen::isValid(arena)){
			// :TODO: log
			printf("ERROR: Arena is too small to hold mesh vertex attribute data\n");
			return false;
		}

		// Update the md structure -> needs to wait until after the arena valid
		// check since we don't want to modify md if we can't perform the load
		md->vertex_count = num_triangles * 3;
		if(buffer_position) { md->attrib_data[aspect.position] = buffer_position; };
		if(buffer_normal  ) { md->attrib_data[aspect.normal  ] = buffer_normal;   };
		if(buffer_color   ) { md->attrib_data[aspect.color   ] = buffer_color;    };
		if(buffer_texcoord) { md->attrib_data[aspect.texcoord] = buffer_texcoord; };

		// Check if there is anything to load from the mesh, if not bail out
		if(buffer_position == nullptr &&
		   buffer_normal   == nullptr &&
		   buffer_color    == nullptr &&
		   buffer_texcoord == nullptr){
			// :TODO: log
			printf("WARN: Nothing to load from mesh file\n");
			return true;
		}

		//printf("Loading attributes from assimp mesh: position: %i, normal: %i, color: %i, texcoord: %i\n",
		//       aspect.position, aspect.normal, aspect.color, aspect.texcoord
		//      );

		for(u32 face_index = 0; face_index < mesh->mNumFaces; ++face_index){
			aiFace* face = &mesh->mFaces[face_index];
			XenAssert(face->mNumIndices == 3, "Expected mesh to be triangulated");

			u32 idx[] = { face->mIndices[0], face->mIndices[1], face->mIndices[2] };

			if(buffer_position){
				for(int i = 0; i < 3; ++i){
					buffer_position[face_index].vertices[i] =
						aiVector3dToXenVec3<real>(mesh->mVertices[idx[i]]);
				}
			}
			if(buffer_normal){
				for(int i = 0; i < 3; ++i){
					buffer_normal[face_index].vertices[i] =
						aiVector3dToXenVec3<real>(mesh->mNormals[idx[i]]);
				}
			}
			if(buffer_texcoord){
				for(int i = 0; i < 3; ++i){
					buffer_texcoord[face_index].vertices[i] =
						aiVector3dToXenVec3<real>(mesh->mTextureCoords[0][idx[i]]).xy;
				}
			}
			if(buffer_color){
				for(int i = 0; i < 3; ++i){
					buffer_color[face_index].vertices[i] =
					  aiColor4dToXenColor4f(mesh->mColors[0][idx[i]]);
				}
			}
		}

		transaction.commit();
		return true;
	}

	bool processAssimpScene(xen::MeshData* md, xen::ArenaLinear& arena, const aiScene* scene){
		// :TODO: this is currently not a very general function...
		// we dont use many of assimps features

		XenAssert(scene->mNumMeshes == 1, "Can currently only handle loading of single mesh");
		XenAssert(scene->HasMeshes(), "Expected to find mesh data in loaded scene");
		XenAssert((scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE) == 0, "Expected successful load");
		XenAssert(scene->mRootNode != nullptr, "Expected scene to have a root node");
		XenAssert(scene->mMeshes[0] != nullptr, "Expected to find a mesh at index 0");

		return processAssimpMesh(md, arena, scene->mMeshes[0]);
	}
}

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

	bool loadMeshFile(MeshData* md, ArenaLinear& arena, const char* path){
		const aiScene* scene = aiImportFile(path,
		                                    aiProcess_Triangulate
		                                    );

		if(!scene){
			// :TODO: log error
			printf("ERROR: Failed to load mesh %s: %s\n", path, aiGetErrorString());
			return false;
		}

		bool result = processAssimpScene(md, arena, scene);
		if(!result){
			// :TODO: log
			printf("ERROR: Failed to load mesh from file: %s\n", path);
		}

		aiReleaseImport(scene);

		return result;
	}
}

#endif
