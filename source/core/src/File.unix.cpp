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
#include <sys/sendfile.h>
#include <fcntl.h>
#include <unistd.h>

namespace xen {
	DateTime getPathModificationTime(const char* path){
		struct stat result;
		if(stat(path, &result) == 0){
			return xen::fromCTime(result.st_mtime);
		} else {
			return xen::DateTime::Epoch;
		}
	}

	bool pathExists(const char* path){
		struct stat tmp;
		return stat(path, &tmp) == 0;
	}

	bool copyFile(const char* in, const char* out){
		int fin  = open(in,  O_RDONLY, 0);

		struct stat stat_in;
		if(fstat(fin, &stat_in) != 0){
			return false;
		}

		int fout = open(out, O_WRONLY | O_CREAT | O_TRUNC, stat_in.st_mode & 0777);

		sendfile(fout, fin, 0, stat_in.st_size);

		close(fout);
		close(fin);
	}
}

#endif
