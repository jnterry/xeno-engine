////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of string helper functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_STRING_CPP
#define XEN_CORE_STRING_CPP

#include <xen/core/String.hpp>

#include <cstdio>

namespace xen {

	const String String::Empty = makeString("");

	String::String(char* string) :
		start(string){
		for(end = string; *end != '\0'; ++end){
			// no-op
		}
	}

	String makeString(char* string){
		return String(string);
	}

	const String makeString(const char* string){
		// We temporarily make the string non-const, then cast it
		// back to const upon returning. This is fine since we know
		// we don't modify the string as part of the constructor
		return String(const_cast<char*>(string));
	}

	u64 stringLength(const char* str){
		const char* end;
		for(end = str; *end != '\0'; ++end){
		}
		return end - str;
	}

	u64 stringLength(xen::String str){
		return str.end - str.start;
	}

	bool endsWith(const char* string, const char* suffix){
		const char* cur_suffix = suffix;
		const char* cur_string = string;

		if(*cur_suffix == '\0' && *cur_string == '\0'){
			return true;
		}

		while(*cur_string != '\0'){
			if(*cur_suffix == *cur_string){
				++cur_suffix;
			} else if (*suffix == *cur_string){
				cur_suffix = &suffix[1];
			} else {
				cur_suffix = suffix;
			}
			++cur_string;
			if(*cur_suffix == '\0' && *cur_string == '\0'){
				return true;
			}
		}
		return false;
	}

	bool startsWith(const char* string, const char* prefix){
		const char* cur_string = string;
		const char* cur_prefix = prefix;

		while(*cur_prefix != '\0'){
			if(*cur_string++ != *cur_prefix++){
				return false;
			}
		}
		return true;
	}
}

#endif
