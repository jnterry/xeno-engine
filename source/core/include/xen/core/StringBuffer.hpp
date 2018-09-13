////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of StringBuffer type and related functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_STRINGBUFFER_HPP
#define XEN_CORE_STRINGBUFFER_HPP

#include <xen/core/String.hpp>
#include <xen/core/intrinsics.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Buffer which can be used for manipulating strings
	/// This is useful for dynamically building a string
	/////////////////////////////////////////////////////////////////////
	struct StringBuffer : public String {
		/// \brief Pointer to first char in the buffer being managed
		/// \note The start of the buffer MAY NOT be equal to the start
		/// of the string - this is so we have a bit of space for pre-prending
		/// another string at the start
		char* buffer_start;

		/// \brief Pointer to last char in the buffer being managed
		char* buffer_end;

		/////////////////////////////////////////////////////////////////////
		/// \brief Constructs a new StringBuffer around some char buffer
		/// and sets its contents to some initial value
		/// \param pre_space The amount of space to reserve before the start of the
		/// string, this acts as a useful hint if you are expecting to prepend
		/// characters onto the front of the string in the buffer
		/////////////////////////////////////////////////////////////////////
		StringBuffer(char* buffer, u64 buffer_size, const char* initial_value, u64 pre_space = 0);

		StringBuffer(char* buffer, u64 buffer_size, xen::String initial_value = xen::String::Empty, u64 pre_space = 0);
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Declares and allocates stack memory for a temporary string buffer
	/////////////////////////////////////////////////////////////////////
	#define XenTempStringBuffer(NAME, SIZE, VALUE) \
		char __xen_temp_str_buffer_ ## NAME[SIZE]; \
		::xen::StringBuffer(__xen_temp_str_buffer_ ## NAME, SIZE, VALUE);

	/////////////////////////////////////////////////////////////////////
	/// \brief Prepends some string before the start of the string in a buffer
	/////////////////////////////////////////////////////////////////////
	void stringPrepend(StringBuffer& buffer, const String& prefix);
	inline void stringPrepend(StringBuffer& buffer, const char* prefix){
		return stringPrepend(buffer, xen::makeString(prefix));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Appends some string after the end of the string in a buffer
	/////////////////////////////////////////////////////////////////////
  void stringAppend (StringBuffer& buffer, const String& suffix);
	inline void stringAppend (StringBuffer& buffer, const char* prefix){
		return stringAppend(buffer, xen::makeString(prefix));
	}

}

#endif
