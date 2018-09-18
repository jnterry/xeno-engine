////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief
///
/// \ingroup
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MODULESRENRASTERIZE_MESH_CPP
#define XEN_MODULESRENRASTERIZE_MESH_CPP

#include <xen/sren/rasterizer3d.hxx>
#include "Mesh.hxx"
#include "ModuleRasterize.hxx"

#include "MeshImpl.cpp"

xsr::RasterizerMesh* xsr::getMeshImpl(xen::Mesh mesh){
	return &xsr::state->mesh_pool.slots[mesh._id].item;
}

#endif
