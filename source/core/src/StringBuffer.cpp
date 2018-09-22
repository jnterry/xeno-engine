////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of functions in StringBuffer.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_STRING_BUFFER_CPP
#define XEN_STRING_BUFFER_CPP

#include <xen/core/StringBuffer.hpp>
#include <xen/core/memory/utilities.hpp>

#include <cstring>
#include <cstdarg>

namespace xen {
	StringBuffer::StringBuffer(char* buffer, u64 buffer_size,
	                           const char* initial_value, u64 pre_space)
		: StringBuffer(buffer, buffer_size, xen::makeString(initial_value), pre_space){
		// no-op
	}

	StringBuffer::StringBuffer(char* buffer, u64 buffer_size,
	                           const xen::String initial_value, u64 pre_space)
		: buffer_start(&buffer[            0]),
		  buffer_end  (&buffer[buffer_size-1])
	{
	  u64 val_length = xen::stringLength(initial_value);
		XenAssert(val_length <= buffer_size, "Cannot copy string into buffer smaller than the string");

		u64 start_char = xen::min(pre_space, buffer_size - val_length - 1);

		for(u64 i = 0; i <= val_length; ++i){ // i <= len as we want to copy '\0'
			buffer[start_char + i] = initial_value[i];
		}

		this->start = &buffer[start_char];
		this->end   = &buffer[start_char + val_length];
	}


	void prependString(StringBuffer& buffer, const String& prefix){
		u64 prefix_len = xen::stringLength(prefix);

		u64 space_start = buffer.start - buffer.buffer_start;
		u64 space_end   = buffer.buffer_end - buffer.end;

		if(space_start < prefix_len){
			// then we need to move the string towards the end of the buffer to make room

			XenAssert(space_start + space_end > prefix_len-1, "No room for prepended string");

			u64 buffer_size = buffer.buffer_end - buffer.buffer_start;
			u64 string_size = xen::stringLength(buffer);

			// Work out where to copy the string to, such that approx 50% of remaining
			// space is on either side of final string to support further prepend operations
			u64 new_string_start = (buffer_size - prefix_len - string_size) / 2;

			char* copy_start = &buffer.buffer_start[new_string_start + prefix_len];

			memmove(copy_start, buffer.start, string_size+1);
			buffer.start = copy_start;
			buffer.end   = &copy_start[string_size];
		}

		// we now have space to do the copy...
		char* new_start = (char*)xen::ptrGetRetreated(buffer.start, prefix_len);
		memcpy(new_start, prefix, prefix_len);
		buffer.start = new_start;
	}

	void appendString (StringBuffer& buffer, const String& suffix){
		u64 suffix_len = xen::stringLength(suffix);

		u64 space_start = buffer.start - buffer.buffer_start;
		u64 space_end   = buffer.buffer_end - buffer.end;

		if(space_end < suffix_len){
			// then we need to move the string towards the start of the buffer to make room

			XenAssert(space_start + space_end > suffix_len-1, "No room for appended string");

			u64 buffer_size = buffer.buffer_end - buffer.buffer_start;
			u64 string_size = xen::stringLength(buffer);

			// Work out where to copy the string to, such that approx 50% of remaining
			// space is on either side of final string to support further prepend operations
			u64 new_string_start = (buffer_size - suffix_len - string_size) / 2;

			char* copy_start = &buffer.buffer_start[new_string_start];

			memmove(copy_start, buffer.start, string_size+1);
			buffer.start = copy_start;
			buffer.end   = &copy_start[string_size];
		}

		// we now have space to do the copy...
		char* new_end = (char*)xen::ptrGetAdvanced(buffer.end, suffix_len);
		memcpy(buffer.end, suffix, suffix_len+1); // +1 for \0
		buffer.end = new_end;
	}

	void appendStringf(StringBuffer& buffer, const char* format, ...){
		va_list args;
		va_start(args, format);

	  int space_end = buffer.buffer_end - buffer.end;

	  int written = vsnprintf(buffer.end, space_end, format, args);

	  XenAssert(written < space_end, "No room for appended string");
	  // :TODO: we should move the string forward in the buffer if there is space
	  // at the front, just as is done with appendString -> written will be the
	  // number of characters that should have been written in total, so we
	  // know total string length

		va_end(args);
	}

	void resetStringBuffer(StringBuffer& buffer, const String old_val){
		buffer.start = old_val.start;
		buffer.end   = old_val.end;
		*buffer.end = '\0';
	}

}

#endif
