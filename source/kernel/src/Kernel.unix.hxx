////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_KERNEL_UNIX_HXX
#define XEN_KERNEL_KERNEL_UNIX_HXX


namespace xke {
	struct DynamicLibrary;

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents and Module currently resident in memory
	/////////////////////////////////////////////////////////////////////
	struct ModuleSource {
		/// \brief The path to the shared library file containing code for module
		const char*     lib_path;

		/// \brief The modification time of the lib the last time it was loaded
		xen::DateTime   lib_modification_time;

		/// \brief Handle returned by dlopen
	  void* lib_handle;
	};
}

#endif
