////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types relating to MetaType system
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTROSPECTION_HPP
#define XEN_CORE_INTROSPECTION_HPP

#include <xen/core/introspection_types.hpp>

#include <typeinfo>
#include <type_traits>
#include <cstdio>

namespace xen {

	// Helper type which allows fetching the xen::MetaType instance by actual
	// type at compile time. This should be specialised for each type represented
	// by the MetaType system. If this is not done a sensible default will be used
	template<typename T>
	struct meta_type {
		constexpr static const xen::MetaType type = { typeid(T).name(), sizeof(T), 0 };
	};


	// Helper type with a print function to pretty print some type with sensible
	// default if not specialised
	template<typename T> struct meta_printer;

	// Determines if some type is atomic. This is any type for which the internal
	// structure cannot be broken down. For example, primitive types such as int,
	// and also unknown types (IE: for which meta_type<T> has not been specialized)
	template<typename T>
	constexpr bool isAtomic() { return meta_type<T>::type.field_count == 0; }

	// Allows setting the field of some object to some value
	// For example:
	// ~setField(MetaType<Vec3<int>>, "x", &obj, 3);~
	template<typename T>
  void setField(const MetaType& type, const char* name, void* object, T value){
		StringHash field_hash = xen::hash(name);
		for(int i = 0; i < type.field_count; ++i){
			if(type.fields[i].name_hash != field_hash){ continue; }

			(*((T*)(&((u08*)object)[type.fields[i].offset])) = value);
			return;
		}
	}
	template<typename T_OBJ, typename T_FIELD>
  void setField(const char* name, void* object, T_FIELD value){
		setField<T_FIELD>(xen::meta_type<T_OBJ>::type, name, object, value);
	}

	template<typename T>
	T* getField(const MetaType& type, const char* name, void* object){
		StringHash field_hash = xen::hash(name);
		for(int i = 0; i < type.field_count; ++i){
			if(type.fields[i].name_hash != field_hash){ continue; }
			return ((T*)(&((u08*)object)[type.fields[i].offset]));
		}
		return nullptr;
	}
	template<typename T_OBJ, typename T_FIELD>
  T_FIELD* getField(const char* name, void* object){
		return getField<T_FIELD>(xen::meta_type<T_OBJ>::type, name, object);
	}
	template<typename T>
	const T* getField(const MetaType& type, const char* name, const void* object){
		StringHash field_hash = xen::hash(name);
		for(int i = 0; i < type.field_count; ++i){
			if(type.fields[i].name_hash != field_hash){ continue; }

			return ((const T*)(&((const u08*)object)[type.fields[i].offset]));
		}
		return nullptr;
	}
	template<typename T_OBJ, typename T_FIELD>
  const T_FIELD* getField(const char* name, const void* object){
		return getField<T_FIELD>(xen::meta_type<T_OBJ>::type, name, object);
	}


	// Pretty prints an object of some known meta type, for example
	// Vec3r{ x: 1.0, y: 2.0, z: 3.0 }
	void printType(const MetaType& type, const void* object, FILE* file = stdout);
	template<typename T>
	inline void printType(const void* object, FILE* file = stdout){
		printType(xen::meta_type<T>::type, object, file);
	}
}

//////////////////////////////////////////////////////////////////////////
// * Primitive meta_type specialisations
//////////////////////////////////////////////////////////////////////////

template<> struct xen::meta_type<u08> {
	constexpr static const xen::MetaType type = xen::MetaType{ "u08", sizeof(u08), 0 };
};
template<> struct xen::meta_type<s08> {
	constexpr static const xen::MetaType type = xen::MetaType{ "s08", sizeof(s08), 0 };
};
template<> struct xen::meta_type<u16> {
	constexpr static const xen::MetaType type = xen::MetaType{ "u16", sizeof(u16), 0 };
};
template<> struct xen::meta_type<s16> {
	constexpr static const xen::MetaType type = xen::MetaType{ "s16", sizeof(s16), 0 };
};
template<> struct xen::meta_type<u32> {
	constexpr static const xen::MetaType type = xen::MetaType{ "u32", sizeof(u32), 0 };
};
template<> struct xen::meta_type<s32> {
	constexpr static const xen::MetaType type = xen::MetaType{ "s32", sizeof(s32), 0 };
};
template<> struct xen::meta_type<u64> {
	constexpr static const xen::MetaType type = xen::MetaType{ "u64", sizeof(u64), 0 };
};
template<> struct xen::meta_type<s64> {
	constexpr static const xen::MetaType type = xen::MetaType{ "s64", sizeof(s64), 0 };
};
template<> struct xen::meta_type<float> {
	constexpr static const xen::MetaType type = xen::MetaType{ "float", sizeof(float), 0 };
};
template<> struct xen::meta_type<double> {
	constexpr static const xen::MetaType type = xen::MetaType{ "double", sizeof(double), 0 };
};
template<> struct xen::meta_type<bool> {
	constexpr static const xen::MetaType type = xen::MetaType{ "bool", sizeof(bool), 0 };
};

#endif
