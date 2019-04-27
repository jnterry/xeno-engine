////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Includes overloads for piping xen match types to a std::ostream
///
/// \ingroup unit_tests
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_TESTS_GRAPHICS_OSTREAMOPERATORS_HPP
#define XEN_TESTS_GRAPHICS_OSTREAMOPERATORS_HPP

#include <iostream>
#include <sstream>

#include "../math/ostream_operators.hpp"

#include <xen/graphics/Image_types.hpp>

#include <catch.hpp>

namespace xen{
	inline std::ostream& operator<< (std::ostream& os, const xen::CubeMapUv& v){
		const char* face_name = "Unknown Face";
		switch(v.face){
		case xen::CubeMap::PositiveX: face_name = "+x"; break;
		case xen::CubeMap::PositiveY: face_name = "+y"; break;
		case xen::CubeMap::PositiveZ: face_name = "+z"; break;
		case xen::CubeMap::NegativeX: face_name = "-x"; break;
		case xen::CubeMap::NegativeY: face_name = "-y"; break;
		case xen::CubeMap::NegativeZ: face_name = "-z"; break;
		}
		return os << "CubeMapUv{ "<<  v.uv << ", " << face_name << "}";
	}

	inline std::ostream& operator<< (std::ostream& os, const xen::CubeMapSamplePoints& v){
		os << "CubeMapSamplePoints: " << std::endl;
		for(int i = 0; i < 4; ++i){
			os << " - " << v.coord[i] << " (" << v.weight[i] << ")" << std::endl;
		}
		return os;
	}
}

#endif
