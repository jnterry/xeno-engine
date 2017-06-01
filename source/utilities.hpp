////////////////////////////////////////////////////////////////////////////////
/// \file utilities.hpp
/// \author Jamie Terry
/// \date 2017/06/01
/// \brief Contains various misc utlities
////////////////////////////////////////////////////////////////////////////////

#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{
	struct ArenaLinear;
}

// :TODO: create general array type? xen::Array
struct FileData{
	u64   size; /// \brief Size of the file in bytes
	u08*  data; /// \brief The contents of the file, length of array = size member
};

/// \brief Loads an entire file into memory and null terminates it
/// \note size field will be 0 if file couldnt be opened
/// \note File's data will be appended to the ArenaLinear
FileData loadFileAndNullTerminate(xen::ArenaLinear& arena, const char* name);

#endif
