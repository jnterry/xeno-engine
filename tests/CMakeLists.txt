use_cxx17()

add_executable(unit-tests
  ${CMAKE_CURRENT_LIST_DIR}/main.cpp

  ${CMAKE_CURRENT_LIST_DIR}/extlibs/catch.hpp

	${CMAKE_CURRENT_LIST_DIR}/core/array.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/RingBuffer.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/minmax.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/bits.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/ArenaPool.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/String.cpp
	${CMAKE_CURRENT_LIST_DIR}/core/StringBuffer.cpp

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
	${CMAKE_CURRENT_LIST_DIR}/math/Plane.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/Ray.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/RayTracing.cpp
	${CMAKE_CURRENT_LIST_DIR}/math/latlong.cpp

	${CMAKE_CURRENT_LIST_DIR}/graphics/Camera.cpp
	${CMAKE_CURRENT_LIST_DIR}/graphics/Image.cpp
)

target_include_directories(unit-tests PRIVATE ${XEN_INCLUDE_DIR})
target_link_libraries(unit-tests xen-graphics)
target_link_libraries(unit-tests xen-math    )
target_link_libraries(unit-tests xen-core    )

target_include_directories(unit-tests PRIVATE ${CMAKE_CURRENT_LIST_DIR}/extlibs)

set_target_properties(unit-tests PROPERTIES FOLDER "examples")
