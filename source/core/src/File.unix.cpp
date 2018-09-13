////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Implementation of unix specific File functionality
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_FILE_UNIX_CPP
#define XEN_CORE_FILE_UNIX_CPP

#include <sys/types.h>
#include <sys/stat.h>

namespace xen {
	DateTime getFileModificationTime(const char* path){
		struct stat result;
		if(stat(path, &result) == 0){
			return xen::fromCTime(result.st_mtime);
		} else {
			return xen::DateTime::Epoch;
		}
	}

	bool doesPathExist(const char* path){
		struct stat tmp;
		return stat(path, &tmp) == 0;
	}
}

#endif
