////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Implementation of windows specific File functionality
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_FILE_WIN_CPP
#define XEN_CORE_FILE_WIN_CPP

#include <xen/windows_header.hxx>
#include <sys/stat.h>
#include <shlwapi.h>

namespace xen {
	DateTime getPathModificationTime(const char* path){
		struct _stat result;
		if(_stat(path, &result) == 0){
			return xen::fromCTime(result.st_mtime);
		} else {
			return xen::DateTime::Epoch;
		}
	}

	bool pathExists(const char* path){
		if(!PathFileExistsA(path)){
			return false;
		}
		return GetFileAttributesA(path) & FILE_ATTRIBUTE_DIRECTORY == 0;
	}

	bool copyFile(const char* in, const char* out){
		return CopyFile(in, out, FALSE);
	}

	bool deleteFile(const char* path){
		return DeleteFileA(path);
	}
}

#endif
