////////////////////////////////////////////////////////////////////////////////
//                        Part of Xeno Engine                                 //
////////////////////////////////////////////////////////////////////////////////
/// \brief Convienience header which includes the correct opengl headers for this
/// platform and also defines a helper macro that can be used to check for opengl
/// errors
///
/// \ingroup gl
////////////////////////////////////////////////////////////////////////////////

#ifndef XEN_GRAPHICS_GL_HEADER_HXX
#define XEN_GRAPHICS_GL_HEADER_HXX

#include <cstdio>

#include <xen/core/intrinsics.hpp>
#include <xen/config.hpp>

#ifdef XEN_OS_WINDOWS
	#include <xen/windows_header.hxx>
	//#include <GL/glew.h>
	#include <GL/gl.h>
#elif defined XEN_OS_UNIX
	#include <GL/glew.h>
	#include <GL/gl.h>
	//#include <GL/glu.h>
#else
	#error GL header not known on this platform
#endif


//#ifndef XEN_DEBUG
//in debug we want to check for gl errors and log them if they occur
	//use this for a gl function that returns something
	#define XEN_CHECK_GL_RETURN(call) (xen::impl::checkGl((call), __LINE__, __FILE__))

	//use this for a gl function that does not return anything
	#define XEN_CHECK_GL(call) ((call), xen::impl::checkGl(__LINE__, __FILE__))
//#else
	//not in debug mode, we dont want any extra overhead!
	//#define XEN_CHECK_GL_RETURN(call) (call)
	//#define XEN_CHECK_GL(call) (call)
//#endif

namespace xen{
	namespace impl{

		inline void checkGl(u32 line, const char* file){
			GLenum err = glGetError();
			if(err == GL_NO_ERROR){ return; }

			const char* err_str = "Unknown Error";

			switch(err){
			case GL_INVALID_ENUM:      err_str = "Invalid value passed as enum argument";           break;
			case GL_INVALID_VALUE:     err_str = "Numeric argument invalid";                        break;
			case GL_INVALID_OPERATION: err_str = "Operation invalid in current state";              break;
			case GL_STACK_OVERFLOW:    err_str = "Requested command would cause a stack overflow";  break;
			case GL_STACK_UNDERFLOW:   err_str = "Requested command would cause a stack underflow"; break;
			case GL_OUT_OF_MEMORY:     err_str = "Out of memory";                                   break;
			}

			//:TODO: log system
			printf("%s|%i: OpenGL Error %i occured: '%s'\n", file, line, err, err_str);
		}

		template<typename T>
		T checkGl(T in, u32 line, const char* file){
			checkGl(line, file);
			return in;
		}
	}
}

#endif
