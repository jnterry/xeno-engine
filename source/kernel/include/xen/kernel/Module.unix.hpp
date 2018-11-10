////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains unix specific macro defintions for modules
///
/// \ingroup kerenel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_UNIX_HPP
#define XEN_KERNEL_UNIX_HPP

#define XenDeclareModule(NAME, INIT, SHUTDOWN, LOAD, UNLOAD, TICK) \
	::xen::Module exported_xen_module = { \
		xen::hash(NAME), \
		INIT, SHUTDOWN, \
		LOAD, UNLOAD, \
		TICK \
	};


#endif
