////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform independent implementation of File system API
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_FILE_CPP
#define XEN_CORE_FILE_CPP

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/File.hpp>

#include <cstdio>

namespace xen{

	FileData loadFileAndNullTerminate(xen::ArenaLinear& arena, const char* path){
		xen::MemoryTransaction transaction(arena);

		//:TODO: fseek end file size deermination is broken when reading non-binary since
		//file size can be different to chars read, as on windows \r\n -> \n when read
		//also if file contains the byte 0 it will be interpreted as end of the returned
		//string when it shouldnt be, this probably only happen when actually loading binary
		//files -> use os specific functions?
		FILE* file = fopen(path, "rb");
		if(!file){
			// :TODO: log -> failed to open file
			printf("FILE: failed to open: %s\n", path);
			return {0};
		}

		u64 arena_space = getBytesRemaining(arena);

		fseek(file, 0, SEEK_END);
		s64 file_size = ftell(file);
		if(file_size < 0){
			// :TODO: log -> failed to determine file size
			printf("FILE: failed to determine size of: %s\n", path);
			return {0};
		}
		fseek(file, 0, SEEK_SET);

		if(arena_space < (u64)file_size + 1){ //safe cast since already checked if file_size < 0
			// :TODO: -> log, not enough space
			printf("FILE: failed allocate space for: %s\n", path);
			return {0};
		}

		u8* data = (u8*)xen::reserveBytes(arena, (u64)file_size + 1);
		fread(data, (u64)file_size, 1, file);
		data[file_size] = '\0';
		fclose(file);

		transaction.commit();
		FileData result;
		result.size     = file_size;
		result.elements = data;
		return result;
	}
}

#endif
