////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains type and functions for representing ring buffers
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_RING_BUFFER_HPP
#define XEN_CORE_RING_BUFFER_HPP

#include <xen/core/intrinsics.hpp>

namespace xen {

/////////////////////////////////////////////////////////////////////
/// \brief Represents a ring buffer which may be treated as a queue
/////////////////////////////////////////////////////////////////////
template<typename T>
struct RingBuffer {
	typedef T TYPE;

	/// \brief Array of length `capacity` containing the elements in the buffer
	T* elements;

	/// \brief The size of the elements array
	u64 capacity;

	/// \brief The number of elements in the ring buffer
	u64 size;

	/// \brief The index of the first element of `elements` which is in use
	u64 front;

	/// \brief The index of the first element of `elements` which is free
	u64 first_free;
};

template<typename T>
u64 size    (RingBuffer<T>& buffer){ return buffer.size;     }
template<typename T>
u64 capacity(RingBuffer<T>& buffer){ return buffer.capacity; }

/////////////////////////////////////////////////////////////////////
/// \brief Appends an element to the end of some RingBuffer. Performs
/// copy of the element to be pushed back using "=" operator
///
/// \return Pointer to the created element, or nullptr if the ring_buffer
/// is full
/////////////////////////////////////////////////////////////////////
template<typename T>
T* pushBack(RingBuffer<T>& buffer, T element) {
	if(buffer.size >= buffer.capacity){ return nullptr; }

	T* result = &buffer.elements[buffer.first_free];

	++buffer.first_free;
	buffer.first_free %= buffer.capacity;

	++buffer.size;

	*result = element;

	return result;
}

/////////////////////////////////////////////////////////////////////
/// \brief Retrieves reference to first element in RingBuffer, without
/// modifing the ring buffer
/////////////////////////////////////////////////////////////////////
template<typename T>
T& peakFront(const RingBuffer<T>& buffer) {
	XenAssert(buffer.size > 0, "Expected at least 1 element in ring buffer");

	return buffer.elements[buffer.front];
}

/////////////////////////////////////////////////////////////////////
/// \brief Retrieves reference to last element in RingBuffer, (IE:
/// that which was most recently pushed) without modifing the ring buffer
/////////////////////////////////////////////////////////////////////
template<typename T>
T& peakBack(const RingBuffer<T>& buffer){
	XenAssert(buffer.size > 0, "Expected at least 1 element in ring buffer");

	if(buffer.first_free == 0){
		return buffer.elements[buffer.capacity-1];
	} else {
		return buffer.elements[buffer.first_free-1];
	}
}

/////////////////////////////////////////////////////////////////////
/// \brief Removes the first element in a RingBuffer returning the element
/// by value
/////////////////////////////////////////////////////////////////////
template<typename T>
T popFront(RingBuffer<T>& buffer){
	XenAssert(buffer.size > 0, "Expected at least 1 element in ring buffer");

	T result = buffer.elements[buffer.front];

	++buffer.front;
	buffer.front %= buffer.capacity;
	--buffer.size;

	return result;
}

/////////////////////////////////////////////////////////////////////
/// \brief Resets a RingBuffer such that all slots are once again free
/// to be used
/////////////////////////////////////////////////////////////////////
template<typename T>
void clear(RingBuffer<T>& buffer){
	buffer.size       = 0;
	buffer.first_free = 0;
	buffer.front      = 0;
}

/// \brief Represents iterator over the elements of some RingBuffer
template<typename T>
struct RingBufferIterator {
	RingBuffer<T>* buffer;
	u64 offset;

	/// \brief Access current element
	T* operator->() {
		return &this->buffer->elements[(buffer->front + this->offset) % buffer->capacity];
	}

	/// \brief Retrieves reference to current element
	T& operator*() {
		return *this->operator->();
	}

	/// \brief Determines if this iterator is valid
	operator bool() {
		return this->offset < buffer->size;
	}

	/// \brief Advances this iterator to the next element of the buffer
	RingBufferIterator& operator++(){
		++this->offset;
	}
};

/// \brief Retrieves an iterator to the first element in some RingBuffer
template<typename T>
RingBufferIterator<T> iterateFirst(RingBuffer<T>& buffer){
	return { &buffer, 0 };
}

}
#endif
