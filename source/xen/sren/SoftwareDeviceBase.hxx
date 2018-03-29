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

#include <xen/core/memory/ArenaLinear.hpp>
#include <xen/core/memory/ArenaPool.hpp>
#include <xen/core/memory/Allocator.hpp>

#include "RenderTargetImpl.hxx"

namespace xen {
	namespace sren {
		class PostProcessor;

		class SoftwareDeviceBase : public xen::GraphicsDevice {
		private:
			xen::Array<PostProcessor*> post_processors;
		protected:
			xen::Allocator*  main_allocator;
			xen::ArenaLinear misc_arena;

			xen::ArenaPool<RenderTargetImpl> render_targets;

			/// \brief Creates a new RenderTarget with the specified size
			RenderTarget      createRenderTarget (Vec2u size, Window* window);

			/// \brief Destroys a RenderTarget freeing all associated resources
			void              destroyRenderTarget(RenderTarget target);

			/// \brief Retrieves the RenderTargetImpl for a particular
			/// RenderTarget handle
			RenderTargetImpl* getRenderTargetImpl(RenderTarget target);

			/// \brief Resizes an existing render target. Note that contents of the
			/// render target will be undefined after the resize, hence clear should
			/// be called and then rendering be performed
			/// \note Safe to call if render target has not yet been initialised, IE:
			/// this will allocate a new set of buffers for an empty target
			void              resizeRenderTarget (RenderTargetImpl* target, Vec2u new_size);
		public:

			SoftwareDeviceBase(xen::Array<PostProcessor*> post_processors);

			virtual ~SoftwareDeviceBase();

			void clear(xen::RenderTarget& target, xen::Color color) override;

			Window* createWindow(Vec2u size, const char* title) override;
			void    destroyWindow(Window* window) override;
			void    swapBuffers(Window* window) override;


		};
	}
}

#endif
