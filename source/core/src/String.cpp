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

	//String::String(char* string) :
	//	start(string){
	//	for(end = string; *end != '\0'; ++end){
	//		// no-op
	//	}
	//}

	String makeString(char* string){
		String result;
		result.start = string;
		for(result.end = string; *result.end != '\0'; ++result.end){
			// no-op
		}
		return result;
	}

	const String makeString(const char* string){
		// We temporarily make the string non-const, then cast it
		// back to const upon returning. This is fine since we know
		// we don't modify the string as part of the constructor
		return makeString(const_cast<char*>(string));
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

	bool endsWith(const xen::String string, const xen::String suffix){
		u64 string_length = stringLength(string);
		u64 suffix_length = stringLength(suffix);

		if(string_length < suffix_length){
			return false;
		}

		for(u64 i = suffix_length; i > 0; --i){
			if(string[string_length - i] != suffix[suffix_length - i]){
				return false;
			}
		}
		return true;
	}

	bool endsWith(const xen::String string, const char* suffix){
		return endsWith(string, xen::makeString(suffix));
	}

	bool endsWith(const char* string, const xen::String suffix){
		return endsWith(string, (const char*)suffix);
	}

	bool endsWith(const char* string, char suffix){
		const char* cur = string;
		while(*cur != '\0'){
			if(cur[0] == suffix && cur[1] == '\0'){
				return true;
			}
			++cur;
		}
		return false;
	}

	bool endsWith(const xen::String string, char suffix){
		return (string.end != string.start) && (string.end[-1] == suffix);
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

	const char* findFirst(const char* haystack, const char* needle){
		// :TODO: This is naive string search O(n^2)
		//
		// look into Knuth-Morris-Pratt or similar
		// (although naive might actually be faster for very small strings as
		//  don't need to pre-compute any tables...)

		for(const char* hcur = haystack; *hcur != '\0'; ++hcur){
			for(int i = 0; true; ++i){
				if(needle[i] == '\0'){
					// then we've found the string
					return hcur;
				}

				if(hcur[i] == '\0'){
					// then needle is longer than remaining haystack
					return nullptr;
				}

				if(hcur[i] != needle[i]){
					break;
				}
			}
		}
		return nullptr;
	}
}

#endif
