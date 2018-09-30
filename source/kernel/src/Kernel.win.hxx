////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains declaration of unix specific Kernel types/functions
///
/// \ingroup kerenl
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_WIN_HXX
#define XEN_KERNEL_KERNEL_WIN_HXX

#include <xen/windows_header.hxx>

namespace xke {
	/////////////////////////////////////////////////////////////////////
	/// \brief Represents and Module currently resident in memory
	/////////////////////////////////////////////////////////////////////
	struct ModuleSource {
		/// \brief The path to the shared library file containing code for module
		const char*     lib_path;

		/// \brief The modification time of the lib the last time it was loaded
		xen::DateTime   lib_modification_time;

		/// \brief The int value appended to lib_path for the actually loaded dll
		/// This is because MSVC cannot write to an open dll, so we must copy it
		/// before loading
		u64 lib_loaded_suffix;

		/// \brief Handle returned by LoadLibrary
	  HMODULE lib_handle;
	};
}

#endif
