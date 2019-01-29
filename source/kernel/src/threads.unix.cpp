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
#include <xen/kernel/log.hpp>

#include <xen/core/atomic_intrinsics.hpp>

#include <sys/sysinfo.h>
#include <unistd.h>
#include <errno.h>

xke::ThreadData xke::thread_data;

// Inserts a new tick work entry into the queue in a thread safe manner
void attachTickWorkEntry(xen::TickWorkHandle index,
                         xen::TickWorkHandle parent_index
                        ){
	xke::TickWorkEntry* parent_entry = &xke::thread_data.tick_work_list[parent_index];

	xen::TickWorkHandle cur_last_child;

	// Keep trying to get grab a slot in the list of children
	do {
		cur_last_child = parent_entry->last_child;
	} while (!xen::sync::compareExchange(&parent_entry->last_child,
	                                     cur_last_child,
	                                     index
	                                    ));

	if(cur_last_child != 0){
		xke::thread_data.tick_work_list[cur_last_child].next_sibling = index;
	}

	// If parent currently has no pending children then set next_child to the new entry
	xen::sync::compareExchange(&parent_entry->next_child,
	                           (xen::TickWorkHandle)0,
	                           index
	                          );
}

void markThreadActive(xen::TickWorkHandle index){

	xke::TickWorkEntry* work_list = xke::thread_data.tick_work_list.elements;

	xen::TickWorkHandle cur  = index;

	for(;;) {
		xen::sync::fetchAndAdd(&work_list[cur].active_threads, (u32)1);
		if(cur == 0){ break; }
		cur = work_list[cur].parent;
	}
}

void markThreadDone(xen::TickWorkHandle index){
	xke::TickWorkEntry* work_list = xke::thread_data.tick_work_list.elements;

	xen::TickWorkHandle cur  = index;

	for(;;) {
		xen::sync::fetchAndSub(&work_list[cur].active_threads, (u32)1);
		if(cur == 0){ break; }
		cur = work_list[cur].parent;
	}

	pthread_mutex_lock    (&xke::thread_data.tick_work_complete_lock);
	pthread_cond_broadcast(&xke::thread_data.tick_work_complete_cond);
	pthread_mutex_unlock  (&xke::thread_data.tick_work_complete_lock);
}


xen::TickWorkHandle xen::createTickWorkGroup(){
	u64 index = xen::sync::fetchAndAdd(&xke::thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < xke::thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &xke::thread_data.tick_work_list[index];

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

	attachTickWorkEntry(index, parent_index);

	return index;
}

xen::TickWorkHandle xen::pushTickWork(xen::TickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){

	void* persist_data = xen::sync::pushBytes(xke::thread_data.tick_work_data, data, data_size);

	u64 index = xen::sync::fetchAndAdd(&xke::thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < xke::thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &xke::thread_data.tick_work_list[index];

	entry->type           = xke::TickWorkEntry::CALLBACK;
	entry->work_data      = persist_data;
	entry->work_func      = work_func;
	entry->parent         = group;
	entry->state          = xke::TickWorkEntry::PENDING;
	entry->next_child     = 0;
	entry->last_child     = 0;
	entry->next_sibling   = 0;
	entry->active_threads = 0;

	attachTickWorkEntry(index, group);

	// Wake up a thread to process the work
	pthread_mutex_lock  (&xke::thread_data.work_available_lock);
	pthread_cond_signal (&xke::thread_data.work_available_cond);
	pthread_mutex_unlock(&xke::thread_data.work_available_lock);

	return index;
}
xen::TickWorkHandle xen::pushTickWork(xen::SimpleTickWorkFunction work_func,
                                      void* data, u64 data_size,
                                      xen::TickWorkHandle group
                                     ){
	void* persist_data = xen::sync::pushBytes(xke::thread_data.tick_work_data, data, data_size);

	u64 index = xen::sync::fetchAndAdd(&xke::thread_data.tick_work_next_free, (u64)1);
	XenAssert(index < xke::thread_data.tick_work_list.size, "Tick work queue full");
	xke::TickWorkEntry* entry = &xke::thread_data.tick_work_list[index];

	entry->type             = xke::TickWorkEntry::SIMPLE_CALLBACK;
	entry->work_data        = persist_data;
	entry->simple_work_func = work_func;
	entry->parent           = group;
	entry->state            = xke::TickWorkEntry::PENDING;
	entry->next_child       = 0;
	entry->last_child       = 0;
	entry->next_sibling     = 0;
	entry->active_threads   = 0;

	attachTickWorkEntry(index, group);

	// Wake up a thread to process the work
	pthread_mutex_lock  (&xke::thread_data.work_available_lock);
	pthread_cond_signal (&xke::thread_data.work_available_cond);
	pthread_mutex_unlock(&xke::thread_data.work_available_lock);

	return index;
}

void processTickWork(xen::TickWorkHandle index){

	//pthread_t this_thread = pthread_self();

	xke::TickWorkEntry* entry = &xke::thread_data.tick_work_list[index];

	if(entry->type != xke::TickWorkEntry::GROUP &&
	   xen::sync::compareExchange(&entry->state,
	                              xke::TickWorkEntry::PENDING,
	                              xke::TickWorkEntry::PROCESSING
	                             )
	   ){

		markThreadActive(index);

		// Then we've locked this work for this thread, do it
		switch(entry->type){
		case xke::TickWorkEntry::CALLBACK:
			entry->work_func(entry->parent, index, entry->work_data);
			break;
		case xke::TickWorkEntry::SIMPLE_CALLBACK:
			entry->simple_work_func(entry->work_data);
			break;
		default:
			XenInvalidCodePath("Missing case statement");
			break;
		}

		entry->state = xke::TickWorkEntry::COMPLETE;

		markThreadDone(index);
	}

	// wait for all sub-tasks to also be completed
	while(entry->next_child != 0){
		xen::TickWorkHandle child = entry->next_child;
		processTickWork(child);
		xen::sync::compareExchange(&entry->next_child,
		                           child,
		                           xke::thread_data.tick_work_list[child].next_sibling
		                          );
	}
}

void xen::waitForTickWork(xen::TickWorkHandle index){
	processTickWork(index);

	pthread_mutex_lock(&xke::thread_data.tick_work_complete_lock);
	while(xke::thread_data.tick_work_list[index].active_threads > 0){
		pthread_cond_wait(&xke::thread_data.tick_work_complete_cond,
		                  &xke::thread_data.tick_work_complete_lock
		                 );
	}
	pthread_mutex_unlock(&xke::thread_data.tick_work_complete_lock);
}

void* kernelThreadFunction(void* thread_index){
	xke::ThreadData*     k_thread_data   = &xke::thread_data;

	xke::THIS_THREAD_INDEX = (xen::ThreadIndex)thread_index;

	XenLogDone("Worker thread ready");

	while(!xke::kernel.stop_requested){

		XenLogTrace("Waiting for work to become available");
		pthread_mutex_lock  (&k_thread_data->work_available_lock);
		pthread_cond_wait   (&k_thread_data->work_available_cond,
		                     &k_thread_data->work_available_lock
		                    );
		pthread_mutex_unlock(&k_thread_data->work_available_lock);
		XenLogTrace("Worker awoken");

		xen::waitForTickWork(0); // wait for work attached to root group
	}

	XenLogInfo("Worker thread terminating...");

	return nullptr;
}

bool xke::initThreadSubsystem(){
	uint num_threads = xke::kernel.settings.thread_count;
	if(num_threads == 0){
	  num_threads = sysconf(_SC_NPROCESSORS_ONLN);
	}

	// :TODO: generare default values in kernel init?
	// Otherwise the setting of defaults is duplicated (eg, this
	// in dummy impl)
	if(xke::kernel.settings.thread_scratch_size == 0){
		xke::kernel.settings.thread_scratch_size = xen::megabytes(16);
	}

	XenLogInfo("Initializing kernel with %i threads", num_threads);

	errno = 0;
	if(0 != pthread_cond_init(&xke::thread_data.work_available_cond, nullptr)){
		XenLogError("Failed to init work_available_cond, errno: %i", errno);
		return false;
	}

	errno = 0;
	if(0 != pthread_mutex_init(&xke::thread_data.work_available_lock,  nullptr)){
		XenLogError("Failed to init work_available_lock, errno: %i", errno);
		return false;
	}

	errno = 0;
	if(0 != pthread_cond_init(&xke::thread_data.tick_work_complete_cond, nullptr)){
		XenLogError("Failed to init tick_work_complete_cond, errno: %i", errno);
		return false;
	}

	errno = 0;
	if(0 != pthread_mutex_init(&xke::thread_data.tick_work_complete_lock,  nullptr)){
	  XenLogError("Failed to init tick_work_complete_lock, errno: %i", errno);
	  return false;
	}

	xke::thread_data.thread_count = num_threads;
	xke::thread_data.threads = xen::reserveTypeArray<pthread_t>(xke::kernel.system_arena, num_threads);
	xke::thread_data.scratch_arenas = xen::reserveTypeArray<xen::ArenaLinear>(xke::kernel.system_arena, num_threads);

	xke::thread_data.tick_work_list.size = 16384;
	xke::thread_data.tick_work_list.elements = (xke::TickWorkEntry*)xke::kernel.root_allocator->allocate(
		                                sizeof(xke::TickWorkEntry) * xke::thread_data.tick_work_list.size);

	xke::thread_data.tick_work_data = xen::createArenaLinear
		(*xke::kernel.root_allocator, xke::thread_data.tick_work_list.size * 128);

	pthread_attr_t attribs;
	pthread_attr_init(&attribs);

	struct sched_param scheduling;
	scheduling.sched_priority = -20; // max priority

	pthread_attr_setschedparam(&attribs, &scheduling);
	pthread_attr_setstacksize (&attribs, xen::megabytes(2));

	void* scratch_space = xke::kernel.root_allocator->allocate(xke::kernel.settings.thread_scratch_size * num_threads);
	if(scratch_space == nullptr){
		XenLogError("Failed to allocate space for thread scratch stores");
		return false;
	}

	for(uint i = 0; i < num_threads; ++i){
		xke::thread_data.scratch_arenas[i].start = xen::ptrGetAdvanced(
			scratch_space, i * xke::kernel.settings.thread_scratch_size);
		xke::thread_data.scratch_arenas[i].end   = xen::ptrGetAdvanced(
			scratch_space, (i+1) * xke::kernel.settings.thread_scratch_size - 1 );
		xke::thread_data.scratch_arenas[i].next_byte = xke::thread_data.scratch_arenas[i].start;
	}

	xke::thread_data.threads[0] = pthread_self();
	xke::THIS_THREAD_INDEX = 0;
	// start at i = 1, thread 0 is the calling thread
	for(u64 i = 1; i < num_threads; ++i){

		if(0 != pthread_create(&xke::thread_data.threads[i],
		                       &attribs,
		                       &kernelThreadFunction,
		                       (void*)i
		                       )){
			XenInvalidCodePath("Cannot recover from thread creation failure");
		}
	}

	pthread_attr_destroy(&attribs);

	return true;
}

bool xke::stopThreadSubsystem(){

	// Wake up all worker threads (ensuring they can notice that we are done)
	pthread_mutex_lock    (&xke::thread_data.work_available_lock);
	pthread_cond_broadcast(&xke::thread_data.work_available_cond);
	pthread_mutex_unlock  (&xke::thread_data.work_available_lock);


	for(u64 i = 1; i < xke::thread_data.thread_count; ++i){
		XenLogInfo("Waiting for termination of worker thread: %u", i);

		errno = 0;
		if(0 != pthread_join(xke::thread_data.threads[i], nullptr)){
			XenLogWarn("Error occurred joining with worker %i, errno: %i", i, errno);
		}

		xen::destroyArenaLinear(*xke::kernel.root_allocator, xke::thread_data.scratch_arenas[i]);
	}

	pthread_mutex_destroy(&xke::thread_data.work_available_lock);
	pthread_cond_destroy (&xke::thread_data.work_available_cond);
	pthread_mutex_destroy(&xke::thread_data.tick_work_complete_lock);
	pthread_cond_destroy (&xke::thread_data.tick_work_complete_cond);

	return true;
}

void xke::preTickThreadSubsystem(){
	// Ensure all worker threads are stopped and waiting on the work_avaialble_cond
	pthread_mutex_lock(&xke::thread_data.work_available_lock);

	xen::resetArena(xke::thread_data.tick_work_data);

	for(xen::ThreadIndex i = 0; i < xke::thread_data.thread_count; ++i){
		xen::resetArena(xke::thread_data.scratch_arenas[i]);
	}

	// Init the root work group (id 0)
	xke::thread_data.tick_work_list[0].parent       = 0;
	xke::thread_data.tick_work_list[0].next_child   = 0;
	xke::thread_data.tick_work_list[0].last_child   = 0;
	xke::thread_data.tick_work_list[0].next_sibling = 0;

	xke::thread_data.tick_work_next_free = 1;

	pthread_mutex_unlock(&xke::thread_data.work_available_lock);
}

xen::ArenaLinear& xen::getThreadScratchSpace(){
	xen::ThreadIndex i = xen::getThreadIndex();
	XenAssert(i < xke::thread_data.thread_count,
	          "getTickScratchSpace called from invalid thread"
	          );

	return xke::thread_data.scratch_arenas[i];
}

#endif
