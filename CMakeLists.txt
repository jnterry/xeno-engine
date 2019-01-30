cmake_minimum_required(VERSION 2.8.1)
project("Xeno Engine")

# Enable grouping projects into folders in generated IDE solution files (eg, visual studio)
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/strict_compile.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/xshm.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/double_precision.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/simd.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/build_type.cmake")

set(XEN_LIB_DIR "${CMAKE_CURRENT_LIST_DIR}/lib")
set(XEN_BIN_DIR "${CMAKE_CURRENT_LIST_DIR}/bin")

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

# Force c++ 11 standard
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
use_cxx11()


# Include function names in backtrace()
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  set (CMAKE_CXX_FLAGS "-g -rdynamic ${CMAKE_CXX_FLAGS}")
endif()

add_subdirectory(extlibs)
add_subdirectory(source)
add_subdirectory(tests)
add_subdirectory(examples)
