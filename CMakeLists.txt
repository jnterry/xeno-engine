cmake_minimum_required(VERSION 3.3)
project("Xeno Engine")

# Enable grouping projects into folders in generated IDE solution files (eg, visual studio)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/strict_compile.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/xshm.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/double_precision.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/simd.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/build_type.cmake")

set(XEN_LIB_DIR "${CMAKE_CURRENT_LIST_DIR}/lib" CACHE "STRING"
	"Where to output built static libraries")
set(XEN_BIN_DIR "${CMAKE_CURRENT_LIST_DIR}/bin" CACHE "STRING"
	"Where to output built binaries and dynamic link libraries")

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY         ${XEN_LIB_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG   ${XEN_LIB_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${XEN_LIB_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY         ${XEN_BIN_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${XEN_BIN_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE ${XEN_BIN_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY         ${XEN_BIN_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${XEN_BIN_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${XEN_BIN_DIR})

set(XEN_EXTLIBS_DIR     "${CMAKE_CURRENT_LIST_DIR}/extlibs")
set(XEN_INCLUDE_DIRS
	${CMAKE_CURRENT_LIST_DIR}/source/core/include/
	${CMAKE_CURRENT_LIST_DIR}/source/math/include/
	${CMAKE_CURRENT_LIST_DIR}/source/kernel/include/
	${CMAKE_CURRENT_LIST_DIR}/source/graphics/include/
	${CMAKE_CURRENT_LIST_DIR}/source/sren/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-window/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-gl/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-sren/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-sren-rasterize/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-sren-raytrace/include/
	${CMAKE_CURRENT_LIST_DIR}/source/module-sren-atom/include/
)

# Macros to force use of particular standard
# Unfortuantely xenogin requires c++17 while assimp requires c++11,
# hence we need to use different standards for these...
macro(use_cxx11)
  if (CMAKE_VERSION VERSION_LESS "3.1")
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "-std=gnu++11 ${CMAKE_CXX_FLAGS}")
 		else ()
 			message(FATAL_ERROR "Unable to enable c++11 support on this platform!")
    endif ()
  else ()
    set (CMAKE_CXX_STANDARD 11)
  endif ()
endmacro(use_cxx11)
macro(use_cxx17)
  if (CMAKE_VERSION VERSION_LESS "3.1")
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "-std=gnu++17 ${CMAKE_CXX_FLAGS}")
 		else ()
 			message(FATAL_ERROR "Unable to enable c++17 support on this platform!")
    endif ()
  else ()
    set (CMAKE_CXX_STANDARD 17)
  endif ()
endmacro(use_cxx17)
use_cxx17()


# Include function names in backtrace()
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_CXX_FLAGS "-g -rdynamic ${CMAKE_CXX_FLAGS}")
endif()

set(XEN_BUILD_MODULES "core;math;kernel;graphics;window;gl" CACHE LIST "List of xeno engine modules which should be built")

add_subdirectory(extlibs)
add_subdirectory(source)

set(XEN_BUILD_UNIT_TESTS TRUE CACHE BOOL "If set then unit test application will be built")
if(${XEN_BUILD_UNIT_TESTS})
	add_subdirectory(tests)
endif()

add_subdirectory(examples)
