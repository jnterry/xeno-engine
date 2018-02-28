////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains definition of a base class for software device renderers
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_SOFTWAREDEVICEBASE_CPP
#define XEN_SREN_SOFTWAREDEVICEBASE_CPP

#include "SoftwareDeviceBase.hxx"
#include "renderer3d.hxx"

namespace xen {
	namespace sren {

		SoftwareDeviceBase::SoftwareDeviceBase(xen::RawImage* image)
			: diffuse_buffer(image) {

		}

		SoftwareDeviceBase::~SoftwareDeviceBase(){
			// no-op
		}

		void SoftwareDeviceBase::clear(xen::RenderTarget target, const xen::Aabb2u& viewport, xen::Color color){
			xen::sren::clear(*this->diffuse_buffer, viewport, color);
		}
	}
}

#endif
