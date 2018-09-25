////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains implementation of WorkQueue related functions in a dummy
/// fashion which simply processes all entries on a single thread
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_WORKQUEUE_DUMMY_HXX
#define XEN_KERNEL_WORKQUEUE_DUMMY_HXX

#include <pthread.h>
#include <xen/core/array_types.hpp>
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

		/// \brief The parent  of this entry
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

		xen::Array<pthread_t> threads;

		xen::Array<TickWorkEntry> tick_work_list;
	  u64                       tick_work_next_free;

		/// \brief Storage for data to be passed to tick_work callbacks
		xen::ArenaLinear          tick_work_data;
	};
}

#endif
