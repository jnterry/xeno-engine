////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definitions of types for representing light sources
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_LIGHT_HPP
#define XEN_GRAPHICS_LIGHT_HPP

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Represents data about some light source in 3d
	/////////////////////////////////////////////////////////////////////
	struct LightSource3d {
		LightSource3d(){}

		enum Type {
			POINT,
			//DIRECTIONAL,
			//SPOT,
			//AREA,
		};

		/// \brief The type of this light source
		Type type;

		/// \brief Parameters which control how quickly light intensity falls of
		/// with distance.
		/// x is constant, y is linear, z is quadratic
		Vec3f attenuation;

		/// \brief The color of the light produced by this light source
	  Color4f color;

		union {
			struct {
				/// \brief The position of a point light
				Vec3r position;
			} point;

			//struct {
			//	Vec3r direction;
			//} directional;

			// struct {
			//  Vec3r position;
			// 	Vec3r direction;
			// 	Angle inner_cone;
			// 	Angle outer_cone;
			// 	/// \brief Falloff of spotlight's cone
			// 	/// x is constant (IE: difference between inner and outer cone
			// 	/// y is linear component
			// 	/// z is quadratic component
			// 	Vec3r cone_falloff;
			// } spot;

			//struct {
			//	Vec3r  position;
			//	Vec3r  direction;
			//	Aabb2r area; /// \brief Area of area light, centred on the light's position
			//} area;
		};
	};
}

#endif
