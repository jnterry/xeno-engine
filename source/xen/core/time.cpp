////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform agnostic implementation for functions in time.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_TIME_CPP
#define XEN_CORE_TIME_CPP

#include <xen/core/time.hpp>
#include <xen/config.hpp>
#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/utilities.hpp>

#include <ctime>

#if XEN_OS_WINDOWS
    #include "time.win.cpp"
#elif XEN_OS_UNIX
    #include "time.unix.cpp"
#else
    #error Time not implemented on this OS
#endif

//:TODO: haven't done rigorous testing to check the platform specific time
//representations are being converted safely - ie, don't wrap, esp for getTimeStamp

namespace xen{
	namespace impl{ std::tm asCTime(const DateTime& dt); } //impl in the platform files

	char* pushDateTimeString(ArenaLinear& arena, DateTime dt, const char* format, u32 align){
		xen::MemoryTransaction transaction(arena);

		char* start = (char*)ptrGetAlignedForward(arena.next_byte, align);
		ptrdiff_t buffer_length = ptrDiff(start, arena.end);
		if(buffer_length > 0){
			size_t bytes_written = formatDateTime(dt, start, getBytesRemaining(arena), format);
			ptrAdvance(&arena.next_byte, bytes_written);

			transaction.commit();
			return start;
		} else {
			// :TODO: log?
			return nullptr;
		}
	}

	size_t formatDateTime(DateTime dt, char* buffer, size_t buffer_length, const char* format){
		std::tm dt_tm = impl::asCTime(dt);

	   auto result = strftime(buffer, buffer_length, format, &dt_tm);
		if(result == 0){
			buffer[0] = '\0';
			return 1;
		} else {
			return result + 1; //+1 for null terminator
		}
	}
}

xen::DateTime operator+(const xen::DateTime& lhs, const xen::Duration& rhs){
	xen::DateTime result = lhs;
	result += rhs;
	return result;
}
xen::DateTime operator-(const xen::DateTime& lhs, const xen::Duration& rhs){
	xen::DateTime result = lhs;
	result -= rhs;
	return result;
}

#endif
