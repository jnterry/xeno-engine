////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Contains various config macros for Xeno Engine
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CONFIG_HPP
#define XEN_CONFIG_HPP

/// \name Platform Macros
///
/// \brief These macros are defined depending on the platform Xeno Engine is being
/// compiled on. Generally they should be automattically detected, but if this fails
/// you will need to compile with one of the these set
///
/// @{

//determine OS
#if !defined(XEN_OS_WINDOWS) && !defined(XEN_OS_UNIX)
	#if defined(_WIN32) || defined(__WIN32__) || defined _MSC_VER
      /// \brief Defined whenever
		#define XEN_OS_WINDOWS 1
	#elif defined(linux) || defined(__linux) || defined(__unix) || defined(__unix__) || defined(unix)
		#define XEN_OS_UNIX 1
	#else
		//Unsupported system
		//note, it could be that the os is supported but the compiler doesn't #define
		//one of the expected values, so we cant automattically determine the OS
		//try defining the one of the XEN_OS_*** #defines manually
		#error This operating system is not supported by Xeno Engine
	#endif
#endif

/// @}

#endif
