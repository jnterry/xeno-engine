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
#include <xen/math/vector.hpp>
#include <xen/core/File.hpp>
#include <xen/graphics/Mesh.hpp>

#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include <assimp/vector3.h>
#include <assimp/color4.h>

namespace {
	/// \brief Recenters a mesh such that its local origin is at the center of
	/// its aabb
	void processMeshVertexPositionsAndComputeBounds(xen::MeshData* mesh,
	                                                xen::MeshLoadFlags flags,
	                                                xen::ArenaLinear& arena){
		u08 pos_attrib_index = findMeshAttrib(mesh, xen::VertexAttribute::Aspect::Position);
		u08 nor_attrib_index = findMeshAttrib(mesh, xen::VertexAttribute::Aspect::Normal);
		if(pos_attrib_index == xen::MeshData::BAD_ATTRIB_INDEX){ return; }

		Vec3r* pbuf = (Vec3r*)mesh->vertex_data[pos_attrib_index];

	  mesh->bounds = xen::computeBoundingBox(pbuf, mesh->vertex_count);

	  Vec3r bounds_size   = xen::getSize  (mesh->bounds);
	  Vec3r bounds_center = xen::getCenter(mesh->bounds);

		bool recenter = ((flags & xen::MeshLoadFlags::CENTER_ORIGIN) &&
		                 (bounds_center != Vec3r::Origin)
		                );

		real max_dim = xen::maxDimension(bounds_size);
		bool rescale  = ((flags & xen::MeshLoadFlags::SCALE_UNIT_SIZE) &&
		                 (max_dim != 1)
		                );

		if(!recenter && !rescale){ return; }

		Vec3r pre_scale    = -bounds_center;
		real  scale_factor = 1;
		if(rescale){
			scale_factor = 1_r / max_dim;
		}
		Vec3r post_scale = -pre_scale;
		if(recenter){
			post_scale = Vec3r::Origin;
		}
		for(u32 i = 0; i < mesh->vertex_count; ++i){
			pbuf[i] += pre_scale;
			pbuf[i] *= scale_factor;
			pbuf[i] += post_scale;
		}

		mesh->bounds.min += pre_scale;
		mesh->bounds.min *= scale_factor;
		mesh->bounds.min += post_scale;
		mesh->bounds.max += pre_scale;
		mesh->bounds.max *= scale_factor;
		mesh->bounds.max += post_scale;

		mesh->bounds = xen::computeBoundingBox(pbuf, mesh->vertex_count);

		if(nor_attrib_index != xen::MeshData::BAD_ATTRIB_INDEX &&
		   flags & xen::MeshLoadFlags::GENERATE_FLAT_NORMALS &&
		   mesh->vertex_data[nor_attrib_index] == nullptr){
			// then we need to generate normals
			XenAssert(mesh->vertex_spec[nor_attrib_index] == xen::VertexAttribute::Normal3r,
			          "Other normal formats not yet supported"
			         );
			Vec3r* nbuf = xen::reserveTypeArray<Vec3r>(arena, mesh->vertex_count);
			mesh->vertex_data[nor_attrib_index] = nbuf;
			xen::computeFlatNormals(mesh->vertex_count, pbuf, nbuf);
		}
	}

	struct VertexAttributeAspects {
		u08 position;
		u08 normal;
		u08 color;
		u08 texcoord;
	};

	VertexAttributeAspects findVertexSpecAspectIndices(const xen::VertexSpec& spec){
		VertexAttributeAspects result = {
			xen::MeshData::BAD_ATTRIB_INDEX,
			xen::MeshData::BAD_ATTRIB_INDEX,
			xen::MeshData::BAD_ATTRIB_INDEX,
			xen::MeshData::BAD_ATTRIB_INDEX,
		};

		for(u32 i = 0; i < spec.size; ++i){
			switch(spec[i].aspect){
			case xen::VertexAttribute::Aspect::Position:
				XenAssert(result.position == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple positions per vertex");
				result.position = i;
				break;
			case xen::VertexAttribute::Aspect::Normal:
				XenAssert(result.normal == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple normals per vertex");
				result.normal = i;
				break;
			case xen::VertexAttribute::Aspect::Color:
				XenAssert(result.color == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple colors per vertex");
				result.color = i;
				break;
			case xen::VertexAttribute::Aspect::TexCoord:
				XenAssert(result.texcoord == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple tex coords per vertex");
				result.texcoord = i;
				break;
			default:
				XenInvalidCodePath("Missing case");
				break;
			}
		}

		return result;
	}

	template<typename T>
	Vec3<T> aiVector3dToXenVec3(aiVector3D v){
		return Vec3<T> { (T)v.x, (T)v.y, (T)v.z };
	}

	xen::Color4f aiColor4dToXenColor4f(aiColor4D c){
		return xen::Color4f { (float)c.r, (float)c.g, (float)c.b, (float)c.a };
	}

	bool processAssimpMesh(xen::MeshData* md, xen::ArenaLinear& arena, const aiMesh* mesh){
		xen::MemoryTransaction transaction(arena);
		VertexAttributeAspects aspect = findVertexSpecAspectIndices(md->vertex_spec);

		XenAssert(mesh->mNumFaces > 0, "Expected mesh to contain some faces");
		XenAssert(mesh->mPrimitiveTypes == aiPrimitiveType_TRIANGLE, "Expected mesh to be triangulated");

		md->primitive_type = xen::PrimitiveType::Triangles;

		int num_triangles = mesh->mNumFaces;

		xen::Triangle3r* buffer_position = nullptr;
		xen::Triangle3r* buffer_normal   = nullptr;
		xen::Triangle4f* buffer_color    = nullptr;
		xen::Triangle2f* buffer_texcoord = nullptr;

		if(aspect.position != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh->HasPositions()){
			buffer_position = xen::reserveTypeArray<xen::Triangle3r>(arena, num_triangles);
		}
		if(aspect.normal   != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh->HasNormals()){
			buffer_normal = xen::reserveTypeArray<xen::Triangle3r>(arena, num_triangles);
		}
		if(aspect.color    != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh->HasVertexColors(0)){
			buffer_color = xen::reserveTypeArray<xen::Triangle4f>(arena, num_triangles);
		}
		if(aspect.texcoord != xen::MeshData::BAD_ATTRIB_INDEX &&
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
		if(buffer_position) { md->vertex_data[aspect.position] = buffer_position; };
		if(buffer_normal  ) { md->vertex_data[aspect.normal  ] = buffer_normal;   };
		if(buffer_color   ) { md->vertex_data[aspect.color   ] = buffer_color;    };
		if(buffer_texcoord) { md->vertex_data[aspect.texcoord] = buffer_texcoord; };

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
						(Vec2f)aiVector3dToXenVec3<real>(mesh->mTextureCoords[0][idx[i]]).xy;
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
	u32 getVertexAttributeSize(VertexAttribute attrib){
		u32 result = 0;

		switch(attrib.type & VertexAttribute::Type::ComponentTypeMask){
		case VertexAttribute::Type::Float  : result = sizeof(float ); break;
		case VertexAttribute::Type::Double : result = sizeof(double); break;
		case VertexAttribute::Type::Byte   : result = sizeof(u08   ); break;
		default:
			XenInvalidCodePath("Unhandled type in getVertexAttributeSize");
			break;
		}

		result *= attrib.type & VertexAttribute::Type::ComponentCountMask;

		return result;
	}


	MeshData* createEmptyMeshData(ArenaLinear& arena, const VertexSpec& spec){
		xen::MemoryTransaction transaction(arena);

		xen::MeshData* result = xen::reserveType<xen::MeshData>(arena);

		if(!xen::isValid(arena)){ return nullptr; }

		result->vertex_spec.size     = spec.size;
		result->vertex_spec.elements = xen::reserveTypeArray<xen::VertexAttribute>(arena, spec.size);
		result->vertex_data          = xen::reserveTypeArray<void*               >(arena, spec.size);

		if(!xen::isValid(arena)){ return nullptr; }

		xen::copyArray<xen::VertexAttribute>(&spec[0], &result->vertex_spec[0], spec.size);
		xen::clearToZero(result->vertex_data, spec.size * sizeof(void*));

		transaction.commit();
		return result;
	}

	bool loadMeshFile(MeshData* md, ArenaLinear& arena, const char* path,
	                  MeshLoadFlags flags){

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

		processMeshVertexPositionsAndComputeBounds(md, flags, arena);

		return result;
	}

	u08 findMeshAttrib(const MeshData* mesh_data, VertexAttribute::Aspect aspect){
		for(u08 i = 0; i < mesh_data->vertex_spec.size; ++i){
			if(mesh_data->vertex_spec[i].aspect == aspect){ return i; }
		}
		return MeshData::BAD_ATTRIB_INDEX;
	}
}

void xen::computeFlatNormals(u64 vertex_count, Vec3r* pbuf, Vec3r* nbuf){
	for(u32 i = 0; i < vertex_count; i += 3){
		xen::Triangle3r* ptri = (xen::Triangle3r*)&pbuf[i];
		Vec3r normal = xen::computeNormal(*ptri);
		nbuf[i+0] = normal;
		nbuf[i+1] = normal;
		nbuf[i+2] = normal;
	}
}

void xen::computeFlatNormals(xen::MeshData* mesh_data){
	VertexAttributeAspects aspects = findVertexSpecAspectIndices(mesh_data->vertex_spec);
	Vec3r* pbuf = (Vec3r*)mesh_data->vertex_data[aspects.position];
	Vec3r* nbuf = (Vec3r*)mesh_data->vertex_data[aspects.normal ];
	computeFlatNormals(mesh_data->vertex_count, pbuf, nbuf);
}

#endif
