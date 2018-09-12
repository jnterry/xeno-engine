////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains public interface for interacting with the file system
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_FILE_HPP
#define XEN_CORE_FILE_HPP

#include <xen/core/array_types.hpp>

namespace xen{
	struct ArenaLinear;

	/// \brief Represents the contents of some file loaded into memory
	typedef xen::Array<u08> FileData;

	/// \brief Loads an entire file into memory and null terminates it
	/// \note size field will be 0 if file couldn't be opened
	/// \note File's data will be appended to the ArenaLinear
	FileData loadFileAndNullTerminate(xen::ArenaLinear& arena, const char* name);
}

#endif
