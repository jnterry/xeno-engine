# :TODO: what about more modern extensions (sse3, avx, etc?)
# :TODO: can we auto detect if these are supported (and then have option to force off even if they are?)

option(XEN_SSE_EXTENSIONS "If set then xeno engine will make use of sse and sse2 instructions" ON)

if(XEN_SSE_EXTENSIONS)
	message("Using SSE/SSE2 extensions")
	add_definitions(-DXEN_USE_SSE=1)
endif()
