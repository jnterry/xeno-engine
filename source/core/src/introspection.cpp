////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of introspection system
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTROSPECTION_CPP
#define XEN_CORE_INTROSPECTION_CPP

#include <xen/core/introspection.hpp>

#include <cstdio>
#include <cstring>

constexpr const xen::MetaType xen::meta_type<u08>::type;
constexpr const xen::MetaType xen::meta_type<s08>::type;
constexpr const xen::MetaType xen::meta_type<u16>::type;
constexpr const xen::MetaType xen::meta_type<s16>::type;
constexpr const xen::MetaType xen::meta_type<u32>::type;
constexpr const xen::MetaType xen::meta_type<s32>::type;
constexpr const xen::MetaType xen::meta_type<u64>::type;
constexpr const xen::MetaType xen::meta_type<s64>::type;
constexpr const xen::MetaType xen::meta_type<float>::type;
constexpr const xen::MetaType xen::meta_type<double>::type;
constexpr const xen::MetaType xen::meta_type<bool>::type;

void doPrintType(const xen::MetaType& type, const void* object, FILE* file, uint indent);

void doPrintQType(const xen::QualifiedMetaType& qtype, const void* object, FILE* file, uint indent){
	if(qtype.pointer_indirection > 0){
		fprintf(file, "%p\n", object);
		return;
	}
	doPrintType(*qtype.base, object, file, indent);
}

void doPrintAtomic(const xen::MetaType& type, const void* object, FILE* file){
	if(&type == &xen::meta_type<u08>::type){
		fprintf(file, "%c (%u)", *((u08*)object), *((u08*)object));
	} else if(&type == &xen::meta_type<s08>::type){
		fprintf(file, "%c (%i)", *((s08*)object), *((s08*)object));
	} else if(&type == &xen::meta_type<u16>::type){
		fprintf(file, "%u", *((u16*)object));
	} else if(&type == &xen::meta_type<s16>::type){
		fprintf(file, "%i", *((s16*)object));
	} else if(&type == &xen::meta_type<u32>::type){
		fprintf(file, "%u", *((u32*)object));
	} else if(&type == &xen::meta_type<s32>::type){
		fprintf(file, "%i", *((s32*)object));
	} else if(&type == &xen::meta_type<u64>::type){
		fprintf(file, "%llu", *((u64*)object));
	} else if(&type == &xen::meta_type<float>::type){
		fprintf(file, "%f", *((float*)object));
	} else if(&type == &xen::meta_type<double>::type){
		fprintf(file, "%f", *((double*)object));
	} else if(&type == &xen::meta_type<bool>::type){
		fprintf(file, "%s", *((bool*)object) ? "true" : "false");
	} else {
		fprintf(file, "[%s %p]", type.name, object);
	}
}

void doPrintType(const xen::MetaType& type, const void* object, FILE* file, uint indent){
	/////////////////////////////////////////////////////////////////////////////
	// Check if type is atomic and act accordingly
	if(xen::isAtomic(type)){
		doPrintAtomic(type, object, file);
		return;
	}

	/////////////////////////////////////////////////////////////////////////////
	// If type only has a few fields, and all are atomic, then print the type as
	// a single like (eg: Vec3s{ x: 1, y: 2, z: 3 })
	if(type.field_count < 4){
		bool all_fields_atomic = true;
		for(int i = 0; i < type.field_count && all_fields_atomic; ++i){
			all_fields_atomic &= (xen::isAtomic(type.fields[i].type.base) |
			                      (type.fields[i].type.pointer_indirection > 0));
		}

		if(all_fields_atomic){
			fprintf(file, "%s { ", type.name);
			for(int i = 0; i < type.field_count; ++i){
				fprintf(file, "%s: ", type.fields[i].name);
				doPrintQType(type.fields[i].type,
				             &(((u08*)object)[type.fields[i].offset]),
				             file,
				             indent + 1);
				if(i < type.field_count-1){ fprintf(file, ", "); }
			}

			fprintf(file, " }");
			return;
		}
	}

	/////////////////////////////////////////////////////////////////////////////
	// Default JSON style printing
	fprintf(file, "%s {\n", type.name);
	int max_len = 0;
	for(int i = 0; i < type.field_count; ++i){
		int len = strlen(type.fields[i].name);
		if(len > max_len){ max_len = len; }
	}

	for(int i = 0; i < type.field_count; ++i) {
		for(uint j = 0; j < indent+1; ++j){  fprintf(file, "  "); }
		fprintf(file, "%-*s : ", max_len, type.fields[i].name);
		doPrintQType(type.fields[i].type,
		             &(((u08*)object)[type.fields[i].offset]),
		             file,
		             indent + 1);
		printf("\n");
	}

  for(uint j = 0; j < indent; ++j){  fprintf(file, "  "); }
	fprintf(file, "}");
}

void xen::printType(const xen::MetaType& type, const void* object, FILE* file){
	doPrintType(type, object, file, 0);
}

#endif
