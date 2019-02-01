////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of StringBuffer type and related functions
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_STRINGBUFFER_HPP
#define XEN_CORE_STRINGBUFFER_HPP

#include <xen/core/String.hpp>
#include <xen/core/intrinsics.hpp>

/////////////////////////////////////////////////////////////////////
/// \brief Declares and allocates stack memory for a temporary string buffer
/////////////////////////////////////////////////////////////////////
#define XenTempStringBuffer(name, size, value)	  \
	char _xen_temp_string_buf_ ## name[size]; \
	::xen::StringBuffer name(_xen_temp_string_buf_ ## name, size, value, size/3);

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
	/// \brief Prepends some string before the start of the string in a buffer
	/////////////////////////////////////////////////////////////////////
	void prependString(StringBuffer& buffer, const String& prefix);
	inline void prependString(StringBuffer& buffer, const char* prefix){
		return prependString(buffer, xen::makeString(prefix));
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Appends some string after the end of the string in a buffer
	/////////////////////////////////////////////////////////////////////
  void appendString (StringBuffer& buffer, const String& suffix);
	inline void appendString (StringBuffer& buffer, const char* prefix){
		return appendString(buffer, xen::makeString(prefix));
	}


	/////////////////////////////////////////////////////////////////////
	/// \brief Appends a string to some buffer using printf style format
	/// string and arguments
	/////////////////////////////////////////////////////////////////////
	void appendStringf(StringBuffer& buffer, const char* format, ...);

	/////////////////////////////////////////////////////////////////////
	/// \brief Resets a string buffer to a previous state
	///
	/// For example:
	/// xen::String old_state = buffer;
	/// xen::appendString(buffer, "test");
	/// xen::resetStringBuffer(buffer, old_state);
	///
	/// \note This function will only reset position of the string within the
	/// buffer but does not change the contents of the buffer, hence it is
	/// only useful if the only changes made since capturing the old state
	/// have occurred before the start or after the end of the old string, this
	/// is guarantied to be the case if only prependString and appendString have
	/// been used to modify the buffer
	///
	/// \note If the old_val was not captured from this buffer then this function
	/// will cause errors as the buffer's internal state will be inconsistent,
	/// pointing at a string which is not within the buffer
	/////////////////////////////////////////////////////////////////////
	void resetStringBuffer(StringBuffer& buffer, const String old_val);

}

#endif
