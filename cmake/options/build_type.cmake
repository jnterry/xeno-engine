################################################################################
# Contains options for changing the build type of the engine
#
# Rather than simply having a release vs debug build, xeno engine allows for
# more fine grained controls, with various debug checks being set independently,
# as well as optimization controls
################################################################################

################################################################################
option(XEN_DEBUG_CHECKS "If set then additional error checking code will be inserted. This may slow down the runtime but will add more infomative error messages. Note that this code does not attempt to fix any errors identified (but may bail out early, thus avoiding further issues such as crashes, infinite loops, etc)" OFF)

if(XEN_DEBUG_CHECKS)
	message("Enabling slow checks")
	add_definitions(-DXEN_DEBUG_CHECKS=1)
endif()
################################################################################


################################################################################
option(XEN_COMPILER_OPTIMIZATIONS "If set the compiler optimizations will be enabled" OFF)

if(XEN_COMPILER_OPTIMIZATIONS)
	message("Enabling optimizations")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()
################################################################################

# :TODO: configure if XenCheckGl should be used
# 0 -> no gl checks
# 1 -> gl error callback
# 2 -> full XenCheckGl after every operation
# option(XEN_DEBUG_GL_CHECKS)

# :TODO:
# Whether to include debugging symbols in the code
# option(XEN_DEBUG_SYMBOLS)

# :TODO: generate this?
set(CMAKE_BUILD_TYPE Debug)
