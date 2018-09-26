////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains interface to kernel logging
///
///
/// \todo :TODO: rather than immediately writing logs to stdout buffer them
/// in some store and write them out as a tickwork generated at the start of
/// each tick
///
/// \todo :TODO: Interface for querying stored log messages for last n ticks
/// -> useful for creating in game print out of log messages
/// (idea would be to store last, eg, 1mb of messages and overwrite in ring
/// buffer like fashion, then have functions to iterate over the stored msgs)
///
/// \todo :TODO: Other output sources -> allow setting filters on what is
/// written to stdout, stderr, files, etc. Ideal might be to have a web client
/// that connects to some port exposed by the kernel to view the messages in
/// real time in a nice filterable GUI
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_LOG_HPP
#define XEN_KERNEL_LOG_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/core/time.hpp>
#include <xen/kernel/threads.hpp>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Enumeration of predefined log levels
	///
	/// Note that the log level is actually just a 8 bit unsigned integer,
	/// hence users of Xeno Engine may define their own log levels if they
	/// so desire, however all messages logged by Xeno Engine will use one
	/// of these levels.
	/////////////////////////////////////////////////////////////////////
	enum LogLevel : u08{
		/////////////////////////////////////////////////////////////////////
		/// \brief Very detailed information that could be useful in debugging
		/// some issue, but is emitted in such high volume (multiple times per
		/// frame) that it is likely to cause noticeable slow down and drown out
		/// other log messages
		/////////////////////////////////////////////////////////////////////
		TRACE = 0,

		/////////////////////////////////////////////////////////////////////
		/// \brief An event of minor importance has occurred but which is not
		/// important enough to be logged as info, such an event only occurs at
		/// most a couple of times per frame
		/////////////////////////////////////////////////////////////////////
		DEBUG = 50,

		/////////////////////////////////////////////////////////////////////
		/// \brief We are in the process of performing a fairly significant event
		/// that occurs significantly less frequently than once a frame, and we
		/// want to log some additional information about the setup process
		/////////////////////////////////////////////////////////////////////
		INFO = 100,

		/////////////////////////////////////////////////////////////////////
		/// \brief Similar to info but indicates that some significant setup has
		/// just been completed, and we want to log some data about what we did,
		/// for example, just initialised opengl, log the version string
		/////////////////////////////////////////////////////////////////////
		DONE = 125,

		/////////////////////////////////////////////////////////////////////
		/// \brief Something unusual occurred which may occasionally be part of
		/// normal program operation, but that seems unlikely, for example, a
		/// font was created with 0 glyph (maybe its a dummy font, but more likely
		/// something went wrong in initialisation)
		/////////////////////////////////////////////////////////////////////
		WARN = 150,

		/////////////////////////////////////////////////////////////////////
		/// \brief A recoverable error occurred which will not prevent the program
		/// from continuing to run, but may yield to non-optimal results (eg,
		/// failed to load some font so using default)
		/////////////////////////////////////////////////////////////////////
		ERROR = 200,

		/////////////////////////////////////////////////////////////////////
		/// \brief A severe error occurred which in development should cause
		/// a break point immediately alerting the programmer to the fact
		/// something needs to be fixed. In production mode execution will
		/// continue as if an error occurred
		/////////////////////////////////////////////////////////////////////
		FATAL_DEV = 250,

		/////////////////////////////////////////////////////////////////////
		/// \brief A fatal error occurred which the application cannot recover
		/// from, hence the kernel will terminate
		/////////////////////////////////////////////////////////////////////
		FATAL = 255,
	};

	struct LogMessage {
		/// \brief The level of this log message
		u08           level;

		/// \brief Index of the thread which generated the message
		ThreadIndex   thread;

		/// \brief The line number of the file where the log message was generated
		u32           line_number;

		/// \brief The file in which the log message was generated
		const char*   file;

		/// \brief The module which generated this message
		const char*   module;

		/// \brief The contents of the log message
		const char*   message;

		/// \brief The system time at which the log message was generated
		xen::DateTime time;
	};

	/////////////////////////////////////////////////////////////////////
	/// \brief Writes a log message
	/////////////////////////////////////////////////////////////////////
	void log(u08 level, const char* file, u32 line, const char* message, ...);
}

#define XenLogTrace(...)    ::xen::log(::xen::LogLevel::TRACE,     __FILE__, __LINE__, __VA_ARGS__)
#define XenLogDebug(...)    ::xen::log(::xen::LogLevel::DEBUG,     __FILE__, __LINE__, __VA_ARGS__)
#define XenLogInfo(...)     ::xen::log(::xen::LogLevel::INFO,      __FILE__, __LINE__, __VA_ARGS__)
#define XenLogDone(...)     ::xen::log(::xen::LogLevel::DONE,      __FILE__, __LINE__, __VA_ARGS__)
#define XenLogWarn(...)     ::xen::log(::xen::LogLevel::WARN,      __FILE__, __LINE__, __VA_ARGS__)
#define XenLogError(...)    ::xen::log(::xen::LogLevel::ERROR,     __FILE__, __LINE__, __VA_ARGS__)
#define XenLogFatalDev(...) ::xen::log(::xen::LogLevel::FATAL_DEV, __FILE__, __LINE__, __VA_ARGS__)
#define XenLogFatal(...)    ::xen::log(::xen::LogLevel::FATAL,     __FILE__, __LINE__, __VA_ARGS__)

#endif
