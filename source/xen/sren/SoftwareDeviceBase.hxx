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
#include <xen/graphics/Image.hpp>

namespace xen {
	namespace sren {
		class SoftwareDeviceBase : public xen::GraphicsDevice {
		protected:
			// :TODO: hack -> we should obey the render target passed into drawing functions
			xen::RawImage* diffuse_buffer;
		public:

			/// \brief Constructs a new software render device which will draw to
			/// specified image -> hack -> should obey the render target passed into
			/// drawing functions
			SoftwareDeviceBase(xen::RawImage* image);

			virtual ~SoftwareDeviceBase();

			void clear(xen::RenderTarget target, const xen::Aabb2u& viewport, xen::Color color) override;
		};
	}
}

#endif
