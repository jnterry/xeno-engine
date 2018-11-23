set(XEN_HEADERS_CORE
	${CMAKE_CURRENT_LIST_DIR}/include/xen/config.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/core/intrinsics.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/core/memory.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/core/memory/Allocator.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/core/memory/ArenaLinear.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/core/memory/utilities.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/random.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/time.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/File.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/array.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/array_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/RingBuffer.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/String.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/core/StringBuffer.hpp
)

set(XEN_SOURCES_CORE
	${CMAKE_CURRENT_LIST_DIR}/impl/xen/windows_header.hxx

  ${CMAKE_CURRENT_LIST_DIR}/src/memory/Allocator.cpp
  ${CMAKE_CURRENT_LIST_DIR}/src/memory/ArenaLinear.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/random.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/time.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/File.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/String.cpp
	${CMAKE_CURRENT_LIST_DIR}/src/StringBuffer.cpp
)
