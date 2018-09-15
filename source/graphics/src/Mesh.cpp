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
	void doComputeFlatNormals(u32 vertex_count, Vec3r* pbuf, Vec3r* nbuf){
		for(u32 i = 0; i < vertex_count; i += 3){
			xen::Triangle3r* ptri = (xen::Triangle3r*)&pbuf[i];
			Vec3r normal = xen::computeNormal(*ptri);
			nbuf[i+0] = normal;
			nbuf[i+1] = normal;
			nbuf[i+2] = normal;
		}
	}

	/// \brief Recenters a mesh such that its local origin is at the center of
	/// its aabb
	void processMeshVertexPositionsAndComputeBounds(xen::MeshData* mesh,
	                                                xen::MeshLoadFlags flags,
	                                                xen::ArenaLinear& arena){
		u08 pos_attrib_index = findMeshAttrib(mesh, xen::VertexAttribute::_AspectPosition);
		u08 nor_attrib_index = findMeshAttrib(mesh, xen::VertexAttribute::_AspectNormal);
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
			doComputeFlatNormals(mesh->vertex_count, pbuf, nbuf);
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
			switch(spec[i] & xen::VertexAttribute::_AspectMask){
			case xen::VertexAttribute::_AspectPosition:
				XenAssert(result.position == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple positions per vertex");
				result.position = i;
				break;
			case xen::VertexAttribute::_AspectNormal:
				XenAssert(result.normal == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple normals per vertex");
				result.normal = i;
				break;
			case xen::VertexAttribute::_AspectColor:
				// :TODO: multiple color channels
				XenAssert(result.color == xen::MeshData::BAD_ATTRIB_INDEX,
				          "We don't support multiple colors per vertex... YET");
				result.color = i;
				break;
			case xen::VertexAttribute::_AspectTexCoord:
				// :TODO: multiple texture channels
				XenAssert(result.texcoord == xen::MeshData::BAD_ATTRIB_INDEX,
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
	u32 getVertexAttributeSize(VertexAttribute::Type type){
		u32 result = 0;

		switch(type & VertexAttribute::_TypeMask){
		case VertexAttribute::_TypeFloat  : result = sizeof(float ); break;
		case VertexAttribute::_TypeDouble : result = sizeof(double); break;
		case VertexAttribute::_TypeByte   : result = sizeof(u08   ); break;
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

		result->vertex_spec.size     = spec.size;
		result->vertex_spec.elements = xen::reserveTypeArray<xen::VertexAttribute::Type>(arena, spec.size);
		result->vertex_data          = xen::reserveTypeArray<void*                     >(arena, spec.size);

		if(!xen::isValid(arena)){ return nullptr; }

		xen::copyArray<xen::VertexAttribute::Type>(&spec[0], &result->vertex_spec[0], spec.size);
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

	u08 findMeshAttrib(const MeshData* mesh_data, VertexAttribute::_Flags aspect){
		for(u08 i = 0; i < mesh_data->vertex_spec.size; ++i){
			if((mesh_data->vertex_spec[i] & VertexAttribute::_AspectMask) == aspect){
				return i;
			}
		}
		return MeshData::BAD_ATTRIB_INDEX;
	}

	void fillMeshAttribArrays(MeshAttribArrays* mesh_geom,
	                          const MeshData*   mesh_data,
	                          Allocator*        allocator,
	                          MeshLoadFlags     flags
	                         ){
		xen::clearToZero(mesh_geom);
		mesh_geom->vertex_count = mesh_data->vertex_count;

		VertexAttributeAspects aspect = findVertexSpecAspectIndices(mesh_data->vertex_spec);


		XenAssert(aspect.position != xen::MeshData::BAD_ATTRIB_INDEX,
		          "Mesh must have position attribute");
		XenAssert(mesh_data->vertex_data[aspect.position] != nullptr,
		          "Data source for position attribute must be non-null"
		         );
		{
			XenAssert((mesh_data->vertex_spec[aspect.position] &
			           xen::VertexAttribute::_TypeMask
			           ) == xen::VertexAttribute::_TypeReal,
			          "Expected position components to be reals"
			          );
			XenAssert((mesh_data->vertex_spec[aspect.position] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 3,
			          "Expected position attribute to have 3 channels"
			          );
			u32 byte_count_pos  = sizeof(Vec3r) * mesh_data->vertex_count;
			mesh_geom->position = (Vec3r*)allocator->allocate(byte_count_pos);
			memcpy(mesh_geom->position, mesh_data->vertex_data[aspect.position], byte_count_pos);
		}

		if(aspect.normal != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh_data->vertex_data[aspect.normal] != nullptr
		  ){
			XenAssert((mesh_data->vertex_spec[aspect.normal] &
			           xen::VertexAttribute::_TypeMask
			          ) == xen::VertexAttribute::_TypeReal,
			          "Expected normal components to be reals"
			         );
			XenAssert((mesh_data->vertex_spec[aspect.normal] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 3,
			          "Expected normal attribute to have 3 channels"
			         );
			u32 byte_count_nor  = sizeof(Vec3r) * mesh_data->vertex_count;
			mesh_geom->normal   = (Vec3r*)allocator->allocate(byte_count_nor);
			memcpy(mesh_geom->normal, mesh_data->vertex_data[aspect.normal], byte_count_nor);
		}

		if(aspect.texcoord != xen::MeshData::BAD_ATTRIB_INDEX &&
		   mesh_data->vertex_data[aspect.texcoord] != nullptr
		  ){

			XenAssert((mesh_data->vertex_spec[aspect.texcoord] &
			           xen::VertexAttribute::_TypeMask
			           ) == xen::VertexAttribute::_TypeFloat,
			          "Expected texcoord components to be reals"
			         );
			XenAssert((mesh_data->vertex_spec[aspect.texcoord] &
			           xen::VertexAttribute::_ComponentCountMask
			           ) == 2,
			          "Expected texcoord attribute to have 2 channels"
			         );
			u32 byte_count_uvs  = sizeof(Vec2f) * mesh_data->vertex_count;
			mesh_geom->uvs   = (Vec2f*)allocator->allocate(byte_count_uvs);
			memcpy(mesh_geom->uvs, mesh_data->vertex_data[aspect.texcoord], byte_count_uvs);
		}

		if(aspect.color != xen::MeshData::BAD_ATTRIB_INDEX &&
		    mesh_data->vertex_data[aspect.color] != nullptr
		  ){
			u32 byte_count_color  = sizeof(xen::Color) * mesh_data->vertex_count;
			mesh_geom->color      = (xen::Color*)allocator->allocate(byte_count_color);

			switch(mesh_data->vertex_spec[aspect.color]){
			case xen::VertexAttribute::Color3f: {
				xen::Color3f* src_buf = (xen::Color3f*)mesh_data->vertex_data[aspect.color];
				for(u32 i = 0; i < mesh_data->vertex_count; ++i){
					mesh_geom->color[i] = xen::Color(src_buf[i]);
				}
				break;
			}
			case xen::VertexAttribute::Color4b: {
				memcpy(mesh_geom->color, mesh_data->vertex_data[aspect.color], byte_count_color);
				break;
			}
			default:
				XenInvalidCodePath("Found bad color format in mesh");
			}
		}

		XenAssert(flags == 0 || flags == MeshLoadFlags::GENERATE_FLAT_NORMALS,
		          "Other flags not yet implemented"
		         );

		if(flags & MeshLoadFlags::GENERATE_FLAT_NORMALS != 0 &&
		   mesh_geom->normal == nullptr &&
		   mesh_geom->vertex_count % 3 == 0){
			mesh_geom->normal = (Vec3r*)allocator->allocate
				(sizeof(Vec3r) * mesh_geom->vertex_count);
			xen::computeFlatNormals(mesh_geom);
		}

	}

	void freeMeshAttribArrays(MeshAttribArrays* mesh,
	                          Allocator* allocator){
		if(mesh->position != nullptr){
			allocator->deallocate(mesh->position);
			mesh->position = nullptr;
		}

		if(mesh->normal != nullptr){
			allocator->deallocate(mesh->normal);
			mesh->normal = nullptr;
		}

		if(mesh->color != nullptr){
			allocator->deallocate(mesh->color);
			mesh->color = nullptr;
		}

		if(mesh->uvs != nullptr){
			allocator->deallocate(mesh->uvs);
			mesh->uvs = nullptr;
		}
	}

}

void xen::computeFlatNormals(xen::MeshData* mesh_data){
	VertexAttributeAspects aspects = findVertexSpecAspectIndices(mesh_data->vertex_spec);
	Vec3r* pbuf = (Vec3r*)mesh_data->vertex_data[aspects.position];
	Vec3r* nbuf = (Vec3r*)mesh_data->vertex_data[aspects.normal ];
	doComputeFlatNormals(mesh_data->vertex_count, pbuf, nbuf);
}

void xen::computeFlatNormals(xen::MeshAttribArrays* mesh){
	doComputeFlatNormals(mesh->vertex_count, mesh->position, mesh->normal);
}


void xen::setMeshAttribArraysData(xen::MeshAttribArrays* mesh,
                                  xen::Allocator& alloc,
                                  const xen::VertexSpec& vertex_spec,
                                  u32 attrib_index,
                                  void* new_data,
                                  u32 start_vertex,
                                  u32 end_vertex
                                 ){

	end_vertex = xen::min(end_vertex, mesh->vertex_count);
	if(end_vertex < start_vertex){ return; }

	void** attrib_data = nullptr;
	switch(vertex_spec[attrib_index]){
	case xen::VertexAttribute::Position3r:
		attrib_data = (void**)&mesh->position;
		break;
	case xen::VertexAttribute::Normal3r:
		attrib_data = (void**)&mesh->normal;
		break;
	case xen::VertexAttribute::Color4b:
		attrib_data = (void**)&mesh->color;
		break;
	default:
		XenInvalidCodePath("Attempt to update unsupported mesh attribute type");
		return;
	}

	u32 attrib_size = xen::getVertexAttributeSize(vertex_spec[attrib_index]);
	if(*attrib_data == nullptr){
		*attrib_data = alloc.allocate(attrib_size * mesh->vertex_count);
		if(start_vertex != 0 || end_vertex != mesh->vertex_count){
			// :TODO: log
			printf("WARN: Updating mesh attrib %i but there is no existing data and "
			       "the new data set is not complete\n", attrib_index);
		}
	}

	memcpy(xen::ptrGetAdvanced(*attrib_data, start_vertex * attrib_size),
	       new_data,
	       (end_vertex - start_vertex) * attrib_size
	       );

}

#endif
