////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of various post processor functions
///
/// \ingroup sren
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_SREN_POSTPROCESSORS_HPP
#define XEN_SREN_POSTPROCESSORS_HPP

#include <xen/sren/FrameBuffer.hpp>
#include <xen/math/geometry_types.hpp>

namespace xen {

	namespace sren {

		/////////////////////////////////////////////////////////////////////
		/// \brief Functor type which represents a Post Processing operation to
		/// perform on a FrameBuffer after rendering has already been performed
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
		  virtual void process(FrameBuffer& fb) = 0;
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief Simple PostProcessor which inverts the colors of the framebuffer
		/////////////////////////////////////////////////////////////////////
		struct PostProcessorInvertColors : public PostProcessor {
			void process(FrameBuffer& fb);
		};

		/////////////////////////////////////////////////////////////////////
		/// \brief PostProcessor which displays the depth buffer at some location
		/// on the screen
		/////////////////////////////////////////////////////////////////////
		struct PostProcessorDisplayDepthBuffer : public PostProcessor {
			/// \brief The area of the screen upon which to present the depth_buffer
			/// Values given as floating point between 0 and 1 representing the fractional
			/// coordinates at which to display the buffer
			/// Hence (0, 0) -> (1, 1) will take up the entire screen, where as
			/// (0,.0 0.5) -> (0.5, 1.0) would take up the top-left quarter of the screen
			xen::Aabb2r screen_region;

			/// \brief Nearest z value camera can see
			real z_near;

			/// \brief Furthest z value camera can see
			real z_far;

			void process(FrameBuffer& fb);
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
