////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains types and functions for dealing with time intervals and the
/// current system time
///
/// There are a number of reasons for wanting high precision measurments of time
/// intervals, for example profiling code section and measuring frame times to
/// use as an input to the game simulation logic. Such measurements are
/// represented as a xen::Duration. Helper functions exist for setting and
/// getting its value as various units.
///
/// C++ does not provide a cross platform way to measure such durations with
/// high precision (except std::chrono - but we're trying to avoid relying on
/// std), hence we use platform specific functions to do this. High precision
/// measurements can be accessed with xen::getTimeStamp()
///
/// There is also a need for getting the local time, eg for recording the times
/// at which log messages are generated, displaying times to the user etc.
/// Again C/C++ does not provide a cross platform way with enough precision
/// (<ctime>'s time() has 1 second resolution, we want more for logs).
///
/// The xen::DateTime type represents these local times, and may be obtained
/// with xen::getLocalTime(), note that this is not as precise as
/// xen::getTimeStamp() and hence should not be used for measuing time intervals.
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_TIME_HPP
#define XEN_CORE_TIME_HPP

#include <xen/core/intrinsics.hpp>

namespace xen{
	struct ArenaLinear;

	/// \brief Represents some period of time, various helpers exist to convert between different units
	struct Duration{
		s64 nanoseconds;

		bool operator==(const Duration& other) const{ return this->nanoseconds == other.nanoseconds; }
		bool operator!=(const Duration& other) const{ return this->nanoseconds != other.nanoseconds; }
		bool operator>=(const Duration& other) const{ return this->nanoseconds >= other.nanoseconds; }
		bool operator<=(const Duration& other) const{ return this->nanoseconds <= other.nanoseconds; }
		bool operator< (const Duration& other) const{ return this->nanoseconds <  other.nanoseconds; }
		bool operator> (const Duration& other) const{ return this->nanoseconds >  other.nanoseconds; }

		Duration operator+(const Duration& rhs) { return Duration{this->nanoseconds + rhs.nanoseconds}; }
		Duration operator-(const Duration& rhs) { return Duration{this->nanoseconds - rhs.nanoseconds}; }
		Duration operator*(real scalar)         { return Duration{(s64)(this->nanoseconds * scalar)};   }
		Duration operator/(real scalar)         { return Duration{(s64)(this->nanoseconds / scalar)};   }

		Duration& operator+=(const Duration& rhs) { this->nanoseconds += rhs.nanoseconds; return *this; }
		Duration& operator-=(const Duration& rhs) { this->nanoseconds -= rhs.nanoseconds; return *this; }
		Duration& operator*=(real scalar) { this->nanoseconds = (s64)( (r64)this->nanoseconds * scalar); return *this; }
		Duration& operator/=(real scalar) { this->nanoseconds = (s64)( (r64)this->nanoseconds / scalar); return *this; }
	};

	/// \brief Returns Duration instance representing time since some fixed point (eg: program start),
	/// uses highest precision clock available. Guarentied to never decrease during single run of
	/// program (at least until s64 wraps which is 2500 or so years). This also holds for multiple
	/// threads (ie: if thread A gets \c time_stamp_1 then thread B gets time_stamp_2,
	/// \c time_stamp_1 <= \c time_stamp_2
	/// \todo :TODO: Check thread statement is actually true
	/// \public \memberof Duration
	Duration getTimeStamp();

	/// \brief Helper class for recording time elapsed since some point
	struct Stopwatch{
		Duration start_time;
		inline Stopwatch() : start_time(getTimeStamp()){}
		inline Duration getElapsedTime(){ return getTimeStamp() - start_time; }

		/// \brief Restarts the stopwatch and returns the elapsed time just before reset
		inline Duration restart(){
			Duration now = getTimeStamp();
			Duration last_start = start_time;
			start_time = now;
			return now - last_start;
		}
	};

	/// \brief Opqaue type representing a calendar date and time
	///
	/// \note We need to use custom operators for comparison and arithmetic, so
	/// this has to be a struct rather than typedef, however we need to store
	/// different data depending on the OS. Using proper opaque type would
	/// require it be referred to by pointer/reference by users of Xeno Engine,
	/// but don't want to force these the be allocated dynamically. Hence this
	/// struct just contains padding data, platform specific code just casts it to
	/// the actual type. This relies on sizeof(DateTime) >= sizeof(T) for all T
	/// where T = platform specific data type (static_asserts are in the platform
	/// specific .cpps)
	struct DateTime{
		u64 _data;

		bool operator==(const DateTime& other){ return _data == other._data; }
		bool operator!=(const DateTime& other){ return _data != other._data; }
		bool operator>=(const DateTime& other);
		bool operator<=(const DateTime& other);
		bool operator< (const DateTime& other);
		bool operator> (const DateTime& other);
	};

	/// \brief Gets a DateTime instance representing the current local time
	/// of the system (ie: including daylight saving, time zone, etc).
	///
	/// \note This isn't guarentied to be very precise, use xen::getTimeStamp
	/// for precise duration measurments, or the helper class xen::Stopwatch
	DateTime getLocalTime();

	/// \brief Formats a DateTime as a string, writting the result to the specified buffer
	/// \param format Desired output format, see <ctime>'s strftime for valid format
	/// specifiers
	/// \return The number of characters written, including the null terminator
	/// \note The resulting string will always be null terminated, if buffer_length > 0
	size_t formatDateTime(DateTime dt, char* buffer, size_t buffer_length, const char* format);

	/// \brief Pushes a formatted date time string onto the arena
	/// \return Pointer to first character of the string, note that the string will always be null terminated
	/// even if there was not room to push the entire string
	char* pushDateTimeString(ArenaLinear& arena, DateTime dt, const char* format, u32 align = alignof(char));

	/// \public \memberof Duration
	template<typename T>
	T asNanoseconds(Duration d){
		return (T)d.nanoseconds;
	}

	/// \public \memberof Duration
	template<typename T>
	T asMicroseconds(Duration d){
		return d.nanoseconds / (T)1000;
	}

	/// \public \memberof Duration
	template<typename T> T
	asMillseconds(Duration d){
		return d.nanoseconds / (T)1000000;
	}

	/// \public \memberof Duration
	template<typename T>
	T asSeconds(Duration d){
		return d.nanoseconds / (T)1000000000;
	}

	/// \public \memberof Duration
	template<typename T>
	T asMinutes(Duration d){
		return asSeconds<T>(d) / (T)60;
	}

	/// \public \memberof Duration
	template<typename T>
	T asHours(Duration d){
		return asSeconds<T>(d) / (T)3600;
	}

	/// \public \memberof Duration
	template<typename T>
	T asDays(Duration d){
		return asHours<T>(d) / (T)24;
	}

	/// \constructor
	/// \public \memberof Duration
   Duration nanoseconds(u64 ns){
	   return Duration{ (s64)(ns) };
	}

	/// \constructor
	/// \public \memberof Duration
	Duration microseconds(u64 us){
		return Duration{ (s64)(us * 1000) };
	}

	/// \constructor
	/// \public \memberof Duration
	Duration milliseconds(u64 ms){
		return Duration{ (s64)(ms * 1000000) };
	}

	/// \constructor
	/// \public \memberof Duration
   Duration seconds(double secs){
	   // do part of multiple as a double to keep precision, but do
	   // part as s64 to ensure we don't lose precision with large doubles
	   return Duration{ ((s64)(secs * 100000.0)) * (s64)10000 };
	}

	/// \constructor
	/// \public \memberof Duration
	Duration minutes(double mins){
		return seconds(mins*60.0);
	}

	/// \constructor
	/// \public \memberof Duration
	Duration hours(double hours){
		return seconds(hours*3600);
	}
}

/// \brief Returns the difference between two DateTime instances as
/// a xen::Duration
xen::Duration operator-(const xen::DateTime& lhs, const xen::DateTime& rhs);

xen::DateTime& operator+=(xen::DateTime& lhs, const xen::Duration& rhs);
xen::DateTime& operator-=(xen::DateTime& lhs, const xen::Duration& rhs);

xen::DateTime operator+(const xen::DateTime& lhs, const xen::Duration& rhs);
xen::DateTime operator-(const xen::DateTime& lhs, const xen::Duration& rhs);

#endif
