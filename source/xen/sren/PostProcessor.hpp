////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of various post processor functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_POSTPROCESSORS_HPP
#define XEN_SREN_POSTPROCESSORS_HPP

#include <xen/sren/Framebuffer.hpp>
#include <xen/math/geometry_types.hpp>

namespace xen {

	namespace sren {

		/////////////////////////////////////////////////////////////////////
		/// \brief Functor type which represents a Post Processing operation to
		/// perform on a Framebuffer after rendering has already been performed
		///
		/// Post processors are a class rather than simply a function pointer so
		/// that they can store state. This state may be configuration for the
		/// PostProcessor (eg, for fog: fog color, distance it starts taking effect,
		/// etc), or be state based on previous frames (eg, for iris brightness
		/// adaptation a moving average of the brightness of previous frames)
		/////////////////////////////////////////////////////////////////////
	  class PostProcessor {
	  public:
		  /// \brief Virtual function which when ran performs the post processing
		  /// step represented by this PostProcessor
		  virtual void process(Framebuffer& fb) = 0;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Simple PostProcessor which inverts the colors of the framebuffer
		/////////////////////////////////////////////////////////////////////
		struct PostProcessorInvertColors : public PostProcessor {
			void process(Framebuffer& fb);
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief PostProcessor which displays the depth buffer at some location
		/// on the screen
		/////////////////////////////////////////////////////////////////////
		struct PostProcessorDisplayDepthBuffer : public PostProcessor {
			/// \brief The area of the screen upon which to present the depth_buffer
			/// Values are in pixels in screen space
			xen::Aabb2u screen_region;

			/// \brief Nearest z value camera can see
			real z_near;

			/// \brief Furthest z value camera can see
			real z_far;

			/// \brief Transparency of the depth buffer overlay
			float alpha;

			void process(Framebuffer& fb);
		};

		// :TODO: more post processors, ideas:
		// - antialiasing
		// - fog
		// - depth of field
		// - outline objects (based on edges found in depth space?)
		// - blurs
		// - bloom
		// - lens flare
		// - eye adaptation to brightness (simulate iris changing size to adjust brightness)
	}

}

#endif
