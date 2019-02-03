#include <xen/core/introspection_types.hpp>
#include <xen/math/vector_types.hpp>
#include <xen/math/geometry_types.hpp>
#include <xen/core/introspection.hpp>

constexpr xen::QualifiedMetaType _xen_qmt_real  = { &xen::meta_type<real>::type, 0 };

// Disable gcc's warning about anonymous structs in unions temporarily...
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

template<> struct xen::meta_type<Vec3r> {
  constexpr static const xen::MetaType type = {
	  "Vec3r", sizeof(Vec3r), 3, {
		  { xen::MetaTypeField::Value, "x", xen::hash("x"), sizeof(real)*0, _xen_qmt_real },
		  { xen::MetaTypeField::Value, "y", xen::hash("y"), sizeof(real)*1, _xen_qmt_real },
		  { xen::MetaTypeField::Value, "z", xen::hash("z"), sizeof(real)*2, _xen_qmt_real },
	  }
  };
};
constexpr const xen::MetaType xen::meta_type<Vec3r>::type;

constexpr xen::QualifiedMetaType _xen_qmt_vec3r = { &xen::meta_type<Vec3r>::type, 0 };

template<> struct xen::meta_type<xen::Aabb3r> {
  constexpr static const xen::MetaType type = {
	  "Aabb3r", sizeof(xen::Aabb3r), 2, {
		  { xen::MetaTypeField::Value, "min", xen::hash("min"), sizeof(Vec3r)*0, _xen_qmt_vec3r },
		  { xen::MetaTypeField::Value, "max", xen::hash("max"), sizeof(Vec3r)*1, _xen_qmt_vec3r },
	  }
  };
};
constexpr const xen::MetaType xen::meta_type<xen::Aabb3r>::type;


#pragma GCC diagnostic pop // re-enable -Wpedantic

int main(int argc, const char** argv){

	Vec3r vec = {1,2,3};

	printf("( %f, %f, %f )\n",
	       *xen::getField<Vec3r, real>("x", &vec),
	       *xen::getField<Vec3r, real>("y", &vec),
	       *xen::getField<Vec3r, real>("z", &vec)
	);

	*xen::getField<real>(xen::meta_type<Vec3r>::type, "y", &vec) = 5.0;
	xen::setField<Vec3r, real>("z", &vec, 8.0);

	printf("( %f, %f, %f )\n",
	       *xen::getField<real>(xen::meta_type<Vec3r>::type, "x", &vec),
	       *xen::getField<real>(xen::meta_type<Vec3r>::type, "y", &vec),
	       *xen::getField<real>(xen::meta_type<Vec3r>::type, "z", &vec)
	);

	xen::printType<Vec3r>(&vec); printf("\n");

	xen::Aabb3r aabb = {
		{ 1.0, 2.0, 3.0 },
		{ 10.0, 20.0, 30.0 },
	};
	xen::printType<xen::Aabb3r>(&aabb);  printf("\n");

	return 0;
}
