////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Utility file containing various string helper methods
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_STRING_HPP
#define XEN_CORE_STRING_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Wrapper around a string with pointer to null-terminator
	/// to make various string operations more efficient
	/////////////////////////////////////////////////////////////////////
	struct String {
		/// \brief Pointer to the first character in the string
		char* start;

		/// \brief Pointer to the null terminator ending the string
		char* end;

		//String(char* string);

		/// \brief Checks whether the string is not empty
		inline operator bool() const {
			return start != nullptr && *start != '\0';
		};

		/// \brief Converts the string to a standard c string
		inline operator char*()             { return start; }

		/// \brief Converts the string to a standard c string
		inline operator const char*() const { return start; }

		/// \brief Represents the empty string
		const static String Empty;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Named constructor which creates a string from a c string.
	///
	/// This is needed since we cannot create a const String from a const char*
	/// using a constructor as c++ doesn't allow "const constructors"
	/////////////////////////////////////////////////////////////////////
	String       makeString(char*       string);
	const String makeString(const char* string);

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines how long a string is
	/////////////////////////////////////////////////////////////////////
	u64 stringLength(const char* str);
	u64 stringLength(xen::String str);

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines if some string ends with some other
	/////////////////////////////////////////////////////////////////////
	bool endsWith(const char*       string, const char*       suffix);
	bool endsWith(const xen::String string, const xen::String suffix);
	bool endsWith(const xen::String string, const char*       suffix);
	bool endsWith(const char*       string, const xen::String suffix);
	bool endsWith(const char*       string, char suffix);
	bool endsWith(const xen::String string, char suffix);

	/////////////////////////////////////////////////////////////////////
	/// \brief Determines if some string starts with some other
	/////////////////////////////////////////////////////////////////////
	bool startsWith(const char* string, const char* prefix);
	inline bool startsWith(const xen::String string, const char* prefix){
		return startsWith((const char*)string, prefix);
	}
	inline bool startsWith(const char* string, const xen::String prefix){
		return startsWith(string, (const char*)prefix);
	}
	inline bool startsWith(const xen::String string, const xen::String prefix){
		return startsWith((const char*)string, (const char*)prefix);
	}
	inline bool startsWith(const char* string, char prefix){
		return *string == prefix;
	}

	typedef u64 StringHash;

	inline StringHash constexpr hash(const char* string){
		return *string == '\0' ? 5381 : (StringHash)(*string) + 33 * hash(&string[1]);
	}

}

#endif
