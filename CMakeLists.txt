cmake_minimum_required(VERSION 2.8.1)
project("Xeno Engine")
set(GLOBAL PROPERTY USE_FOLDERS ON)

include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/strict_compile.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/xshm.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/double_precision.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/options/simd.cmake")

# :TODO: have some way of configuring this
set(CMAKE_BUILD_TYPE Debug)

set(XEN_LIB_DIR "${CMAKE_CURRENT_LIST_DIR}/lib")
set(XEN_BIN_DIR "${CMAKE_CURRENT_LIST_DIR}/bin")

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY         ${XEN_LIB_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG   ${XEN_LIB_DIR})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE ${XEN_LIB_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY         ${XEN_LIB_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG   ${XEN_LIB_DIR})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE ${XEN_LIB_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY         ${XEN_BIN_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${XEN_BIN_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${XEN_BIN_DIR})

set(XEN_EXTLIBS_DIR     "${CMAKE_CURRENT_LIST_DIR}/extlibs")
set(XEN_INCLUDE_DIR     "${CMAKE_CURRENT_LIST_DIR}/source/")

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

add_subdirectory(extlibs)
add_subdirectory(source)
add_subdirectory(tests)
add_subdirectory(examples)
