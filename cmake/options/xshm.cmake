if(UNIX)
	option(XEN_NO_XSHM_EXTENSION "If set then it is assumed that the x shared memory extension is not available for software render targets" OFF)

	if(XEN_NO_XSHM_EXTENSION)
		message("Disabling XSHM support")
		add_definitions(-DXEN_NO_XSHM_EXTENSION=1)
	endif()

endif()
