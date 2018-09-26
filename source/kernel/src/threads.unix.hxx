////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains decleration of platform specific types for implementing
/// thread related functions for systems with pthread library (unix, mac)
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_THREADS_UNIX_HXX
#define XEN_KERNEL_THREADS_UNIX_HXX

#include <pthread.h>
#include <xen/core/array_types.hpp>
#include <xen/core/atomic_intrinsics.hpp>
#include <xen/core/memory/ArenaLinear.hpp>

namespace xke {

	struct TickWorkEntry {
		enum Type {
			GROUP,
			SIMPLE_CALLBACK,
			CALLBACK,
		};

		enum State {
			PENDING,
			PROCESSING,
			COMPLETE,
		};


		Type type;

		union {
			xen::TickWorkFunction       work_func;
			xen::SimpleTickWorkFunction simple_work_func;
		};

		State state;

		void* work_data;

		/// \brief How many threads are currently working on this
		/// entry or its decedents
		u32 active_threads;

		/// \brief The parent of this entry
		xen::TickWorkHandle parent;

		/// \brief Next child needing to be processed
		xen::TickWorkHandle next_child;

		/// \brief Last child in the list of children of this entry
		xen::TickWorkHandle last_child;

		/// \brief Next sibling in the list of siblings to process
		xen::TickWorkHandle next_sibling;
	};

	struct ThreadData {
		pthread_cond_t  work_available_cond;
		pthread_mutex_t work_available_lock;

		pthread_cond_t  tick_work_complete_cond;
		pthread_mutex_t tick_work_complete_lock;

		/// \brief Number of threads controlled by the kernel, including the
		// master thread
		xen::ThreadIndex thread_count;

		/// \brief array of pthread_t instances, length = thread_count
		pthread_t* threads;

		/// \brief array of thread scratch spaces, length = thread_count
		xen::ArenaLinear* scratch_arenas;

		xen::Array<TickWorkEntry> tick_work_list;
	  u64                       tick_work_next_free;

		/// \brief Storage for data to be passed to tick_work callbacks
		xen::ArenaLinear          tick_work_data;
	};

	/// \brief Global thread data
	extern ThreadData thread_data;
}

#endif
