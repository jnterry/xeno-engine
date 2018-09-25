////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform specific implementation of thread related functions
/// for systems with pthread library
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_THREADS_UNIX_CPP
#define XEN_KERNEL_THREADS_UNIX_CPP

#include "threads.hxx"
#include "Kernel.hxx"

#include <xen/core/atomic_intrinsics.hpp>

#include <sys/sysinfo.h>
#include <unistd.h>
#include <errno.h>

// Inserts a new tick work entry into the queue in a thread safe manner
void attachTickWorkEntry(xen::Kernel& kernel,
                         xen::TickWorkHandle index,
                         xen::TickWorkHandle parent_index
                        ){
	//printf("About to insert work entry %i, parent: %i\n", index, parent_index);
	xke::TickWorkEntry* parent_entry = &kernel.thread_data.tick_work_list[parent_index];

	xen::TickWorkHandle cur_last_child;

	// Keep trying to get grab a slot in the list of children
	do {
		cur_last_child = parent_entry->last_child;
	} while (!xen::sync::compareExchange(&parent_entry->last_child,
	                                     cur_last_child,
	                                     index
	                                    ));

	if(cur_last_child != 0){
		kernel.thread_data.tick_work_list[cur_last_child].next_sibling = index;
	}

	// If parent currently has no pending children then set next_child to the new entry
	xen::sync::compareExchange(&parent_entry->next_child,
	                           (xen::TickWorkHandle)0,
	                           index
	                          );
}

void markThreadActive(xen::Kernel& kernel,
                      xen::TickWorkHandle index
                     ){

	xke::TickWorkEntry* work_list = kernel.thread_data.tick_work_list.elements;

	xen::TickWorkHandle cur  = index;

	for(;;) {
		xen::sync::fetchAndAdd(&work_list[cur].active_threads, (u32)1);
		if(cur == 0){ break; }
		cur = work_list[cur].parent;
	}
}

void markThreadDone(xen::Kernel& kernel,
                    xen::TickWorkHandle index
                    ){
	xke::TickWorkEntry* work_list = kernel.thread_data.tick_work_list.elements;

	xen::TickWorkHandle cur  = index;

	for(;;) {
		xen::sync::fetchAndSub(&work_list[cur].active_threads, (u32)1);
		if(cur == 0){ break; }
		cur = work_list[cur].parent;
	}

	pthread_mutex_lock    (&kernel.thread_data.tick_work_complete_lock);
	pthread_cond_broadcast(&kernel.thread_data.tick_work_complete_cond);
	pthread_mutex_unlock  (&kernel.thread_data.tick_work_complete_lock);
}


xen::TickWorkHandle xen::createTickWorkGroup(xen::Kernel& kernel){
	u64 index = xen::sync::fetchAndAdd(&kernel.thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < kernel.thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &kernel.thread_data.tick_work_list[index];

	// :TODO: nested groups? -> pass this value in
	// should just work as TickWork callback can push further TickWork which
	// become children of it
	xen::TickWorkHandle parent_index = 0;

	entry->type           = xke::TickWorkEntry::GROUP;
	entry->parent         = parent_index;
	entry->next_child     = 0;
	entry->last_child     = 0;
	entry->next_sibling   = 0;
	entry->active_threads = 0;

	attachTickWorkEntry(kernel, index, parent_index);

	return index;
}

xen::TickWorkHandle xen::pushTickWork(xen::Kernel& kernel,
                                      xen::TickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){

	void* persist_data = xen::sync::pushBytes(kernel.thread_data.tick_work_data, data, data_size);

	u64 index = xen::sync::fetchAndAdd(&kernel.thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < kernel.thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &kernel.thread_data.tick_work_list[index];

	entry->type           = xke::TickWorkEntry::CALLBACK;
	entry->work_data      = persist_data;
	entry->work_func      = work_func;
	entry->parent         = group;
	entry->state          = xke::TickWorkEntry::PENDING;
	entry->next_child     = 0;
	entry->last_child     = 0;
	entry->next_sibling   = 0;
	entry->active_threads = 0;

	attachTickWorkEntry(kernel, index, group);

	// Wake up a thread to process the work
	pthread_mutex_lock  (&kernel.thread_data.work_available_lock);
	pthread_cond_signal (&kernel.thread_data.work_available_cond);
	pthread_mutex_unlock(&kernel.thread_data.work_available_lock);

	return index;
}
xen::TickWorkHandle xen::pushTickWork(xen::Kernel& kernel,
                                      xen::SimpleTickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	void* persist_data = xen::sync::pushBytes(kernel.thread_data.tick_work_data, data, data_size);

	u64 index = xen::sync::fetchAndAdd(&kernel.thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < kernel.thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &kernel.thread_data.tick_work_list[index];

	entry->type             = xke::TickWorkEntry::SIMPLE_CALLBACK;
	entry->work_data        = persist_data;
	entry->simple_work_func = work_func;
	entry->parent           = group;
	entry->state            = xke::TickWorkEntry::PENDING;
	entry->next_child       = 0;
	entry->last_child       = 0;
	entry->next_sibling     = 0;
	entry->active_threads   = 0;

	attachTickWorkEntry(kernel, index, group);

	// Wake up a thread to process the work
	pthread_mutex_lock  (&kernel.thread_data.work_available_lock);
	pthread_cond_signal (&kernel.thread_data.work_available_cond);
	pthread_mutex_unlock(&kernel.thread_data.work_available_lock);

	return index;
}

void processTickWork(xen::Kernel& kernel, xen::TickWorkHandle index){

	//pthread_t this_thread = pthread_self();

	//printf("!!! %p | Waiting for work entry: %i\n", this_thread, index);
	xke::TickWorkEntry* entry = &kernel.thread_data.tick_work_list[index];

	if(entry->type != xke::TickWorkEntry::GROUP &&
	   xen::sync::compareExchange(&entry->state,
	                              xke::TickWorkEntry::PENDING,
	                              xke::TickWorkEntry::PROCESSING
	                             )
	   ){

		markThreadActive(kernel, index);

		// Then we've locked this work for this thread, do it
		//printf("!!! %p | -- Performing work entry: %i\n", this_thread, index);

		switch(entry->type){
		case xke::TickWorkEntry::CALLBACK:
			entry->work_func(kernel, index, entry->work_data);
			break;
		case xke::TickWorkEntry::SIMPLE_CALLBACK:
			entry->simple_work_func(entry->work_data);
			break;
		default:
			XenInvalidCodePath("Missing case statement");
			break;
		}

		entry->state = xke::TickWorkEntry::COMPLETE;

		//printf("!!! %p | -- Completed work entry: %i\n", this_thread, index);

		markThreadDone(kernel, index);
	}

	// wait for all sub-tasks to also be completed
	while(entry->next_child != 0){
		xen::TickWorkHandle child = entry->next_child;
		processTickWork(kernel, child);
		xen::sync::compareExchange(&entry->next_child,
		                           child,
		                           kernel.thread_data.tick_work_list[child].next_sibling
		                          );
	}

	//printf("!!! %p | Returning from wait for work entry: %i\n", this_thread, index);
}

void xen::waitForTickWork(xen::Kernel& kernel, xen::TickWorkHandle index){
	processTickWork(kernel, index);

	pthread_mutex_lock(&kernel.thread_data.tick_work_complete_lock);
	while(kernel.thread_data.tick_work_list[index].active_threads > 0){
		pthread_cond_wait(&kernel.thread_data.tick_work_complete_cond, &kernel.thread_data.tick_work_complete_lock);
	}
	pthread_mutex_unlock(&kernel.thread_data.tick_work_complete_lock);
}


struct KernelPerThreadData {
	xen::Kernel* kernel;
	u32 index;
};

void* kernelThreadFunction(void* data){
	KernelPerThreadData* per_thread_data = (KernelPerThreadData*)data;
	xke::ThreadData*     k_thread_data   = &per_thread_data->kernel->thread_data;

	xke::THIS_THREAD_INDEX = per_thread_data->index;

	printf("Started kernel thread %u\n", xen::getThreadId());

	while(!per_thread_data->kernel->stop_requested){

		//printf("### %2i | Waiting for work\n", per_thread_data->index);

		pthread_mutex_lock  (&k_thread_data->work_available_lock);
		pthread_cond_wait   (&k_thread_data->work_available_cond, &k_thread_data->work_available_lock);
		pthread_mutex_unlock(&k_thread_data->work_available_lock);

		//printf("### %2i | Checking for outstanding work...\n", per_thread_data->index);

		xen::waitForTickWork(*per_thread_data->kernel, 0); // wait for work attached to root group

		//printf("### %2i | Completed outstanding work, going to sleep...\n", per_thread_data->index);
	}

	return nullptr;
}

bool xke::initThreadSubsystem(xen::Kernel* kernel){
	uint num_cores = kernel->settings.thread_count;
	if(num_cores == 0){
	  num_cores = sysconf(_SC_NPROCESSORS_ONLN);
	}
	printf("Initializing kernel with %i threads\n", num_cores);

	errno = 0;
	if(0 != pthread_cond_init(&kernel->thread_data.work_available_cond, nullptr)){
		printf("Failed to work_available cond, errno: %i\n", errno);
		return false;
	}

	errno = 0;
	if(0 != pthread_mutex_init(&kernel->thread_data.work_available_lock,  nullptr)){
		printf("Failed to init work_available mutex, errno: %i\n", errno);
	}

	errno = 0;
	if(0 != pthread_cond_init(&kernel->thread_data.tick_work_complete_cond, nullptr)){
		printf("Failed to tick_work_complete_cond, errno: %i\n", errno);
		return false;
	}

	errno = 0;
	if(0 != pthread_mutex_init(&kernel->thread_data.tick_work_complete_lock,  nullptr)){
		printf("Failed to init tick_work_complete_mutex, errno: %i\n", errno);
	}

	kernel->thread_data.threads.size = num_cores;
	kernel->thread_data.threads.elements = xen::reserveTypeArray<pthread_t>(kernel->system_arena, num_cores);

	kernel->thread_data.tick_work_list.size = 16384;
	kernel->thread_data.tick_work_list.elements = (xke::TickWorkEntry*)kernel->root_allocator->allocate(
		         sizeof(xke::TickWorkEntry) * kernel->thread_data.tick_work_list.size);

	kernel->thread_data.tick_work_data = xen::createArenaLinear
		(*kernel->root_allocator, kernel->thread_data.tick_work_list.size * 128);

  KernelPerThreadData* th_data = xen::reserveTypeArray<KernelPerThreadData>(kernel->system_arena, num_cores);
	pthread_attr_t attribs;
	pthread_attr_init(&attribs);

	struct sched_param scheduling;
	scheduling.sched_priority = -20; // max priority

	pthread_attr_setschedparam(&attribs, &scheduling);
	pthread_attr_setstacksize (&attribs, xen::megabytes(8));

	kernel->thread_data.threads.elements[0] = pthread_self();
	xke::THIS_THREAD_INDEX = 0;
	// start at i = 1, thread 0 is the calling thread
	for(int i = 1; i < num_cores; ++i){
		th_data[i].kernel = kernel;
		th_data[i].index  = i;

		if(0 != pthread_create(&kernel->thread_data.threads.elements[i],
		                       &attribs,
		                       &kernelThreadFunction,
		                       &th_data[i]
		                       )){
			XenInvalidCodePath("Cannot recover from thread creation failure");
		}
	}

	pthread_attr_destroy(&attribs);

	return true;
}

bool xke::stopThreadSubsystem(xen::Kernel* kernel){

	// Wake up all worker threads (ensuring they can notice that we are done)
	pthread_mutex_lock    (&kernel->thread_data.work_available_lock);
	pthread_cond_broadcast(&kernel->thread_data.work_available_cond);
	pthread_mutex_unlock  (&kernel->thread_data.work_available_lock);


	for(u64 i = 0; i < kernel->thread_data.threads.size; ++i){
		printf("Waiting for termination of thread: %lu\n", i);

		errno = 0;
		if(0 != pthread_join(kernel->thread_data.threads[i], nullptr)){
			printf("Error waiting for termination of thread: %lu\n", i);
			return false;
		}
	}

	pthread_mutex_destroy(&kernel->thread_data.work_available_lock);
	pthread_cond_destroy (&kernel->thread_data.work_available_cond);
	pthread_mutex_destroy(&kernel->thread_data.tick_work_complete_lock);
	pthread_cond_destroy (&kernel->thread_data.tick_work_complete_cond);

	return true;
}

void xke::preTickThreadSubsystem(xen::Kernel* kernel){
	// Ensure all worker threads are stopped and waiting on the work_avaialble_cond
	pthread_mutex_lock(&kernel->thread_data.work_available_lock);

	xen::resetArena(kernel->thread_data.tick_work_data);

	// Init the root work group (id 0)
	kernel->thread_data.tick_work_list[0].parent       = 0;
	kernel->thread_data.tick_work_list[0].next_child   = 0;
	kernel->thread_data.tick_work_list[0].last_child   = 0;
	kernel->thread_data.tick_work_list[0].next_sibling = 0;

	kernel->thread_data.tick_work_next_free = 1;

	pthread_mutex_unlock(&kernel->thread_data.work_available_lock);
}

#endif
