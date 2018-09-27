////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_DYNAMICLIBRARY_UNIX_HXX
#define XEN_KERNEL_DYNAMICLIBRARY_UNIX_HXX

namespace xke {
	// under unix the dynamic library handle is just a void*, this type
	// isn't needed, we will just cast the void* to DynamicLibrary
	struct DynamicLibrary {};
}

#endif
