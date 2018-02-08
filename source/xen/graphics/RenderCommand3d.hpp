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
#include <xen/math/vector_types.hpp>
#include <xen/math/matrix_types.hpp>
#include <xen/graphics/Color.hpp>

namespace xen{

	struct RenderCommand3d {
		enum Types {
			/// \brief Draws a set of points defined by `vertices` member
			POINTS,

			/// \brief Draws a set of points defined by `vertices` member
			/// by joining subsequent pairs of points
			LINES,

			/// \brief Draws a set of points defined by `vertices` member
			/// by joining adjacent points
			LINE_STRIP,

			/// \brief Draws a set of triangles defined by `vertices` member
			/// by grouping every 3 vertices
			TRIANGLES,

			COUNT,
		};

		/// \brief Defines the type of render command represented
		u8 type;

		/// \brief The diffuse color to use
		Color color;

		/// \brief Matrix to transform from world space to model space
		Mat4r model_matrix;

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
