////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \file Renderer3d.hpp
/// \author Jamie Terry
/// \date 2018/01/25
/// \brief Contains types for defining 3d render commands
///
/// \ingroup graphics
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_RENDERER3D_HPP
#define XEN_GRAPHICS_RENDERER3D_HPP

#include <xen/core/intrinsics.hpp>
#include <xen/math/Vector.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{

	struct RenderCommand3d {
		enum Types {
			/// \brief Draws a set of points defined by `vertices` member
			POINTS,

			/// \brief Draws a set of points defined by `vertices` member
			/// by joining adjacent points
			LINE_STRIP,

			COUNT,
		};

		/// \brief Defines the type of render command represented
		u8 type;

		/// \brief The diffuse color to use
		Color color;

		/// \brief Extra type dependent data
		union {
			struct {
				Vec3r* verticies;
				u32    count;
			} verticies;
		};
	};

}

#endif