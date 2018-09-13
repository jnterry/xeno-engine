////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains linux specific implementation of functions in time.hpp
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#include <time.h>
#include <errno.h>

//:TODO: unix timespec is u64 for seconds and u64 for nanoseconds, increase DateTime size?

static_assert(sizeof(xen::DateTime) <= sizeof(u64),
              "Platform specific DateTime type must be able to fit in public type"
             );

namespace xen{
	namespace impl{
		std::tm asCppTime(const DateTime& dt){
			time_t seconds_since_epoch = dt._data / (u64)1000000000;
			tm* time = localtime(&seconds_since_epoch);
			return *time;
		}
	}

	Duration getTimeStamp(){
		timespec time;
		if(clock_gettime(CLOCK_MONOTONIC_RAW, &time) == 0){
			return xen::seconds(time.tv_sec) + xen::nanoseconds(time.tv_nsec);
		} else {
			// :TODO: log
			// xen::log::write(XenFatal("OS does not support MONTONIC_RAW clock, errno: %i", errno, "xen.time"));
			return Duration { (s64)0x7FFFFFFFFFFFFFFF };
		}
	}


	DateTime getLocalTime(){
		timespec time;
		if(clock_gettime(CLOCK_REALTIME, &time) == 0){
			DateTime dt;
			dt._data = (xen::seconds(time.tv_sec) + xen::nanoseconds(time.tv_nsec)).nanoseconds;
			return dt;
		} else {
			// :TODO: log
			// xen::log::write(XenFatal("OS does not support REALTIME clock, errno: %i", errno, "xen.time"));
			return DateTime();
		}
	}

	DateTime fromCTime(time_t t){
		DateTime dt;
		dt._data = (xen::seconds(t)).nanoseconds;
		return dt;
	}
}

xen::Duration operator-(const xen::DateTime& lhs, const xen::DateTime& rhs){
	return xen::Duration { (s64)(lhs._data - rhs._data) };
}

xen::DateTime& operator+=(xen::DateTime& lhs, const xen::Duration& rhs){
	lhs._data += rhs.nanoseconds;
	return lhs;
}

xen::DateTime& operator-=(xen::DateTime& lhs, const xen::Duration& rhs){
	lhs._data -= rhs.nanoseconds;
	return lhs;
}
