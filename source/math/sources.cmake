set(XEN_HEADERS_MATH
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/utilities.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/angle.hpp

  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vector_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vector.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/vector_arithmetic.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/vector_operations.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/swizzles.hpp

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/matrix_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/matrix.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_arithmetic.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_operations.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/impl/matrix_transforms.hpp

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/quaternion_types.hpp
  ${CMAKE_CURRENT_LIST_DIR}/include/xen/math/quaternion.hpp

	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/geometry_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/geometry.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/plane_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/plane.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vertex_group_types.hpp
	${CMAKE_CURRENT_LIST_DIR}/include/xen/math/vertex_group.hpp
)

# :TODO: -> can't we make this header only?
set(XEN_SOURCES_MATH
  ${CMAKE_CURRENT_LIST_DIR}/src/quaternion.cpp
)
