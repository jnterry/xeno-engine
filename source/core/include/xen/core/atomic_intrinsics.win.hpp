////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains various wrappers around compiler specific intrinsics for
/// performing atomic manipulations of variables
///
/// \ingroup core
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_CORE_ATOMICINTRINSICS_WIN_HPP
#define XEN_CORE_ATOMICINTRINSICS_WIN_HPP

#include <intrin.h>

namespace xen {

	/////////////////////////////////////////////////////////////////////
	/// \brief Contains functions which modify types in an atomic manner,
	/// thus ensuring that the functions are thread-safe in that any number
	/// of atomic functions may be called on the same variable simultaneously
	/// and the results will be well defined
	/////////////////////////////////////////////////////////////////////
	namespace sync {

		/////////////////////////////////////////////////////////////////////
		/// \brief Atomically fetches the value of some variable, and then adds
		/// some amount to the variable's value. Note that the returned value is
		/// that of the variable BEFORE the addition occurred
		/////////////////////////////////////////////////////////////////////
		template <typename T>
		inline T fetchAndAdd(T* variable, T val){
			return _InterlockedExchangeAdd(variable, val);
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Atomically fetches the value of some variable, and then subtracts
		/// some amount from the variable's value. Note that the returned value is
		/// that of the variable BEFORE the subtraction occurred
		/////////////////////////////////////////////////////////////////////
		template <typename T>
		inline T fetchAndSub(T* variable, T val){
		  return _InterlockedExchangeAdd(variable, -val);
		}

		/////////////////////////////////////////////////////////////////////
		/// \brief Atomically updates the value of some variable if it is currently
		/// equal to some expected value
		/// \param variable The variable to modify
		/// \param old_val  The expected current value for the variable, if the
		/// variable is not equal to this then this function will not modify
		/// variable, and will return false
		/// \param new_val  The value to write to variable, if it's value is equal
		/// to cur_val
		/////////////////////////////////////////////////////////////////////
		inline bool compareExchange(void** variable, void* cur_val, void* new_val){
			return (void*)_InterlockedCompareExchange(reinterpret_cast<volatile long*>(variable),
			                                          (sptr)new_val, (sptr)cur_val) == cur_val;
		}
		template <typename T>
		inline bool compareExchange(T* variable, T cur_val, T new_val){
				_InterlockedCompareExchange(variable, new_val, cur_val) == cur_val;
		}



	}
}

#endif
