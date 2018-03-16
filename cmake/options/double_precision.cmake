option(XEN_DOUBLE_PRECISION "If set then xeno engine is compiled with double precision support, that is, real is a typedef of double rather than float" OFF)

if(XEN_DOUBLE_PRECISION)
	message("Using double precision")
	add_definitions(-DXEN_USE_DOUBLE_PRECISION=1)
endif()
