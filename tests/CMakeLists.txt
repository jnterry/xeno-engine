add_executable(xen_unit_tests
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp

  ${CMAKE_CURRENT_LIST_DIR}/extlibs/catch.hpp

	${CMAKE_CURRENT_LIST_DIR}/core/array.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/minmax.cpp

  ${CMAKE_CURRENT_LIST_DIR}/math/ostream_operators.hpp
	${CMAKE_CURRENT_LIST_DIR}/math/utilities.cpp
  ${CMAKE_CURRENT_LIST_DIR}/math/Angle.cpp
  ${CMAKE_CURRENT_LIST_DIR}/math/Vector.cpp
  ${CMAKE_CURRENT_LIST_DIR}/math/Matrix.cpp
  ${CMAKE_CURRENT_LIST_DIR}/math/Quaternion.cpp
  ${CMAKE_CURRENT_LIST_DIR}/math/Rotation3d.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/Aabb.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/LineSegment.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/Triangle.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/Ray.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/RayTracing.cpp

	${CMAKE_CURRENT_LIST_DIR}/graphics/Camera.cpp
)


target_include_directories(xen_unit_tests PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(xen_unit_tests xen-graphics)
target_link_libraries(xen_unit_tests xen-util    )
target_link_libraries(xen_unit_tests xen-math    )
target_link_libraries(xen_unit_tests xen-core    )

target_include_directories(xen_unit_tests PRIVATE ${CMAKE_CURRENT_LIST_DIR}/extlibs)
