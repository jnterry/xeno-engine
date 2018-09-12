set(XEN_HEADERS_MATH
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/utilities.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/angle.hpp

  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vector_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vector.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/vector_arithmetic.hxx
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/vector_operations.hxx
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/swizzles.hxx

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/matrix_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/matrix.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_arithmetic.hxx
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_operations.hxx
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_transforms.hxx

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/quaternion_types.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/quaternion.hpp

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/geometry_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/geometry.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vertex_group_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vertex_group.hpp
)

# :TODO: -> can't we make this header only?
set(XEN_SOURCES_MATH
  ${CMAKE_CURRENT_LIST_DIR}/src/quaternion.cpp
)
