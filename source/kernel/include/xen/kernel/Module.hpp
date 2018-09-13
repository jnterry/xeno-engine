////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Declaration of module type and related functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_MODULE_HPP
#define XEN_KERNEL_MODULE_HPP

namespace xen {

	struct Kernel;

  /////////////////////////////////////////////////////////////////////
	/// \brief Represents the source for some Module
	/////////////////////////////////////////////////////////////////////
	struct Module {
		typedef bool (*InitFunction    )(Kernel& k);
		typedef void (*ShutdownFunction)(Kernel& k);

		/// \brief Function which should be called exactly once in order to perform
		/// any initial setup of this module, should be performed before any other
		/// functionality of this module is used
		InitFunction init;

		/// \brief Function which should be called exactly once in order to free
		/// any resources allocated/used etc by this module
		ShutdownFunction shutdown;
	};
}

#endif
