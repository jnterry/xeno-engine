////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Includes windows.h and cleans up some of their mess
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_WINDOWS_HEADER_HPP
#define XEN_WINDOWS_HEADER_HPP

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <Windowsx.h>
#undef ERROR //conflicts with xen::log::ERROR

#endif
