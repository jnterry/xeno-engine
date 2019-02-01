////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains types relating to MetaType system
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_INTROSPECTION_TYPES_HPP
#define XEN_CORE_INTROSPECTION_TYPES_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/String.hpp>

namespace xen {
	struct MetaType;

	// Disable gcc's warning about anonymous structs in unions temporarily...
	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wpedantic"

  //////////////////////////////////////////////////////////////////////////
	/// \breif A QualifiedMetaType represents a some type with additional meta data
	/// such as if is declared const, as a pointer, etc
	//////////////////////////////////////////////////////////////////////////
	struct QualifiedMetaType{
		///< The MetaType that this qualifies
		const MetaType* base;

		union{
			struct{
				u08
					is_const            : 1, ///< Whether the const keyword is used
					is_static           : 1, ///< Whether the static keyword is used
					is_reference        : 1, ///< Whether it is a reference
					is_volatile         : 1, ///< Whether the volatile keyword is used
					is_mutable          : 1, ///< Whether the mutable keyword is used
					pointer_indirection : 3; ///< Number of *s, max 8, (int is 0, int** is 2)
			};

			///< 8-bit unsigned type that represents the values of all the qualifiers,
			///< in union with the actual qualifiers, eg, isConst, pointerLevel, etc
		   u08 qualifiers;
		};
	};

	/// \breif A MetaTypeField holds all the data about a single field in a
	/// MetaType, including the actual type of data, the field's name, its
	/// offset in the type, etc
	struct MetaTypeField{
		/// \breif Enumeration of the kinds of MetaTypeFields
		/// \note Kind is a bit of an odd name, but there's already too many uses
		/// of 'type' flying around in the MetaType system
		enum Kind{
			/// \brief Field storing single instance of some type, for example:
			/// \code
			/// int thing;
			/// \endcode
			Value,

			/// \brief A static array is an array of some type whose length is known
			/// at compile time, for example, the following declares a STATIC_ARRAY of
			/// length 10:
			/// \code
			/// int data[10];
			/// \endcode
			StaticArray,

			/// \brief A dynamic array is an array of some type whose length is only
			/// known and may vary at runtime, it is common to have another variable
			/// to track the actual length of such an array.
			///
			/// For example, the following is a typical declaration of a DYNAMIC_ARRAY
			/// of ints:
			/// \code
			/// int* data;
			/// size_t data_count;
			/// \endcode
		  DynamicArray,
		};

		Kind kind;

		const char* name;

		// Hash of ~name~
		StringHash name_hash;

		/// \brief Number of bytes into the type this field is found at
		u32 offset;

		/// \brief The type of the field
		QualifiedMetaType type;

		union {
			/// \brief Fixed size if kind is StaticArray
			u64            length;

			/// \brief Pointer to the field storing the length of a DynamicArray
			/// or nullptr if this information is not known
			MetaTypeField* type_field;
		};
	};

	struct MetaType {
		/// \brief The name of the type being represented
		const char* name;

		/// \brief The size in bytes of this type
		u32 size;

		/// \brief Number of fields
		u16 field_count;

		/// \brief First element of fields list, length = field_count
		MetaTypeField fields[];
	};

	#pragma GCC diagnostic pop // re-enable -Wpedantic
}

#endif
