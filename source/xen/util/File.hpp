////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains public interface for interacting with the file system
///
/// \ingroup util
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_UTIL_FILE_HPP
#define XEN_UTIL_FILE_HPP

namespace xen{
	struct ArenaLinear;

	// :TODO: create general array type? xen::Array
	struct FileData{
		u64   size; /// \brief Size of the file in bytes
		u08*  data; /// \brief The contents of the file, length of array = size member
	};

	/// \brief Loads an entire file into memory and null terminates it
	/// \note size field will be 0 if file couldn't be opened
	/// \note File's data will be appended to the ArenaLinear
	FileData loadFileAndNullTerminate(xen::ArenaLinear& arena, const char* name);
}

#endif
