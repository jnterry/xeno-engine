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
#include <xen/core/time.hpp>

namespace xen{
	struct ArenaLinear;

	/// \brief Represents the contents of some file loaded into memory
	typedef xen::Array<u08> FileData;

	/// \brief Loads an entire file into memory and null terminates it
	/// \note size field will be 0 if file couldn't be opened
	/// \note File's data will be appended to the ArenaLinear
	FileData loadFileAndNullTerminate(xen::ArenaLinear& arena, const char* name);

	/////////////////////////////////////////////////////////////////////
	/// \brief Retrieves the last modification time for some file,
	/// will return {0}
	/////////////////////////////////////////////////////////////////////
	DateTime getPathModificationTime(const char* path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Checks if some path exists
	/////////////////////////////////////////////////////////////////////
	bool pathExists(const char* path);

	/////////////////////////////////////////////////////////////////////
	/// \brief Makes a copy of a file
	/////////////////////////////////////////////////////////////////////
	bool copyFile(const char* in, const char* out);

	/////////////////////////////////////////////////////////////////////
	/// \brief Deletes a file or system link
	/// \return True if the path does not exist now that this function has
	/// returned, hence if the path did not exist before this function
	/// returns true and behaves as no-op
	/////////////////////////////////////////////////////////////////////
	bool deleteFile(const char* path);
}

#endif
