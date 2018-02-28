////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of a base class for software device renderers
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICEBASE_HXX
#define XEN_SREN_SOFTWAREDEVICEBASE_HXX

#include <xen/graphics/GraphicsDevice.hpp>

namespace xen {
	namespace sren {
		class SoftwareDeviceBase : public GraphicsDevice {
		public:
			void clear(RenderTarget target, const xen::Aabb2u& viewport, xen::Color color);
		}
	}
}

#endif
