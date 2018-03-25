////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains extra functions for axis aligned bounding box above and
/// beyond those defined for standard geometry types
///
/// \ingroup math
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_MATH_AABB_HXX
#define XEN_MATH_AABB_HXX

namespace xen {
	namespace impl{
		template<u32 T_DIM, typename T>
		void movePointInto(Vec<T_DIM, T>& point, const Vec<T_DIM, T>& other_point, const Aabb<T_DIM, T>& aabb){
			Vec<T_DIM, T> dir = other_point - point;

			for(u32 dim = 0; dim < T_DIM; ++dim){
				if(dir[dim] != 0){
					if(point[dim] < aabb.min[dim] && dir[dim] > 0){
						point += dir * ((aabb.min[dim] - point[dim]) / dir[dim]);
					}
					if(point[dim] > aabb.max[dim] && dir[dim] < 0){
						point += dir * ((aabb.max[dim] - point[dim]) / dir[dim]);
					}
				}
			}
		}
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Code representing the position of a point compared to an Aabb
	/// see: https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
	/////////////////////////////////////////////////////////////////////
	struct PointOutCode : public xen::BitField<u08, 6> {
		using BitField::BitField; // use constructors of parent

		enum Values {
			/// \brief Point is inside the Aabb
			INSIDE    = 0,

			/// \brief Point is to the left of the Aabb (x too small)
			LEFT      = 1,

			/// \brief Point is to the left of the Aabb (x too big)
			RIGHT     = 2,

			/// \brief Point is below of the Aabb (y too small)
			DOWN      = 4,

			/// \brief Point is above of the Aabb (y too big)
			UP        = 8,

			/// \brief Point is behind the Aabb (z too small)
			BEHIND    = 16,

			/// \brief Point is in front of the Aabb (z too big)
			INFRONT   = 32,
		};
	};

	template <typename T>
	PointOutCode computePointOutCode(Aabb2<T> a, Vec2<T> p){
		PointOutCode result = PointOutCode::INSIDE;

		if(p.x < a.min.x){ result |= PointOutCode::LEFT;  }
		if(p.x > a.max.x){ result |= PointOutCode::RIGHT; }
		if(p.y < a.min.y){ result |= PointOutCode::DOWN;  }
		if(p.y > a.max.y){ result |= PointOutCode::UP;    }

		return result;
	}

	template <typename T>
	PointOutCode computePointOutCode(Aabb3<T> a, Vec3<T> p){
		PointOutCode result = PointOutCode::INSIDE;

		if(p.x < a.min.x){ result |= PointOutCode::LEFT;    }
		if(p.x > a.max.x){ result |= PointOutCode::RIGHT;   }
		if(p.y < a.min.y){ result |= PointOutCode::DOWN;    }
		if(p.y > a.max.y){ result |= PointOutCode::UP;      }
		if(p.z < a.min.z){ result |= PointOutCode::BEHIND;  }
		if(p.z > a.max.z){ result |= PointOutCode::INFRONT; }

		return result;
	}

	/////////////////////////////////////////////////////////////////////
	/// \brief Returns n-dimensional vector representing the size of some
	/// aabb along each dimension
	///
	/// \public \memberof Aabb
	/////////////////////////////////////////////////////////////////////
	template<u32 T_DIM, typename T>
	Vec<T_DIM, T> getSize(Aabb<T_DIM, T> aabb){
		return aabb.max - aabb.min;
	}

	template<u32 T_DIM, typename T>
	Aabb<T_DIM, T> makeAabbFromMinAndSize(Vec<T_DIM, T> min, Vec<T_DIM, T> size){
		return { min, min + size };
	}

	template<typename T>
	Aabb2<T> makeAabbFromMinAndSize(T mx, T my, T sx, T sy) {
		return makeAabbFromMinAndSize(xen::mkVec(mx, my), xen::mkVec(sx, sy));
	}

	template<typename T>
	Aabb3<T> makeAabbFromMinAndSize(T mx, T my, T mz, T sx, T sy, T sz) {
		return makeAabbFromMinAndSize(xen::mkVec(mx, my, mz), xen::mkVec(sx, sy. sz));
	}

	template<u32 T_DIM, typename T>
	Aabb<T_DIM, T>& addPoint(Aabb<T_DIM, T>& aabb, Vec<T_DIM, T> p){
		aabb.min = xen::min(aabb.min, p);
		aabb.max = xen::max(aabb.max, p);
		return aabb;
	}
}

#endif
