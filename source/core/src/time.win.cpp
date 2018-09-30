////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains windows specific implementation of functions in time.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#include <xen/windows_header.hxx>
#include <ctime>

#include <xen/core/time.hpp>

static_assert(sizeof(xen::DateTime) <= sizeof(FILETIME),
              "Platform specific DateTime type must be able to fit in public type"
             );

namespace xen{
	namespace impl{
		std::tm asCppTime(const DateTime& dt){
			SYSTEMTIME sys_time;
			FileTimeToSystemTime((FILETIME*)&dt._data, &sys_time);

			std::tm time;
			time.tm_sec   = sys_time.wSecond;
			time.tm_min   = sys_time.wMinute;
			time.tm_hour  = sys_time.wHour;
			time.tm_mday  = sys_time.wDay;
			time.tm_wday  = sys_time.wDayOfWeek;
			time.tm_mon   = sys_time.wMonth - 1;
			time.tm_year  = sys_time.wYear - 1900;
			time.tm_isdst = -1;

			return time;
		}
	}

	DateTime fromCTime(time_t t){
		// adapted from: https://docs.microsoft.com/en-us/windows/desktop/sysinfo/converting-a-time-t-value-to-a-file-time
		DateTime dt;

		FILETIME* ftime = (FILETIME*)&dt._data;

	  LONGLONG t_val = Int32x32To64(t, 10000000) + 116444736000000000;
    ftime->dwLowDateTime = (DWORD) t_val;
    ftime->dwHighDateTime = t_val >>32;

    return dt;
	}

	Duration getTimestamp(){
		LARGE_INTEGER time, frequency;
		QueryPerformanceFrequency(&frequency);
		QueryPerformanceCounter(&time);

		return Duration{ (time.QuadPart * 1000000000) / frequency.QuadPart };
	}


	DateTime getLocalTime(){
		// we want to store date time as a FILETIME (since its the most compact +
		// allows for easy comparisons, just a u64), however while there is a
		// GetSystemTimeAsFileTime there is no GetLocalTimeAsFileTime, we have to
		// GetLocalTime as a SYSTEMTIME, then convert to a FILETIME
		DateTime result;
		SYSTEMTIME sys_time;

		GetLocalTime(&sys_time);
		SystemTimeToFileTime(&sys_time, (FILETIME*)&result);

		return result;
	}
}

xen::Duration operator-(const xen::DateTime& lhs, const xen::DateTime& rhs){
	// lhs and rhs are really FILETIMEs, which are just u64's with number
	// of 100-nanosecond intervals since Jan 1, 1601

	// these could wrap as converting (u64 - 64)*100 -> s64, but its unlikely we'll every
	// be comparing DateTimes that far apart (291 years apart to wrap)

	// :TODO: can we fix this?

	return { (s64)( (*((u64*)&lhs) - *((u64*)&rhs)) * 100 ) };
}

xen::DateTime& operator+=(xen::DateTime& lhs, const xen::Duration& rhs){
	*((u64*)&lhs) += (xen::asNanoseconds<u64>(rhs) / (u64)100);
	return lhs;
}

xen::DateTime& operator-=(xen::DateTime& lhs, const xen::Duration& rhs){
	*((u64*)&lhs) -= (xen::asNanoseconds<u64>(rhs) / (u64)100);
	return lhs;
}
