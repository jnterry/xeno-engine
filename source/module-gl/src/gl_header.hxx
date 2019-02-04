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
#include <xen/kernel/log.hpp>

#ifdef XEN_OS_WINDOWS
	#include <xen/windows_header.hxx>
	#include <GL/glew.h>
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

// :TODO: replace above with these shorter versions...
#define XGL_CHECK(call)    XEN_CHECK_GL(call)
#define XGL_CHECKRET(call) XEN_CHECK_GL_RETURN(call)

namespace xen{
	namespace impl{

		inline void checkGl(u32 line, const char* file){
			GLenum err = glGetError();
			if(err == GL_NO_ERROR){ return; }

			const char* err_str = "Unknown Error";

			switch(err){
			case GL_INVALID_ENUM:      err_str = "GL_INVALID_ENUM";      break;
			case GL_INVALID_VALUE:     err_str = "GL_INVALID_VALUE";     break;
			case GL_INVALID_OPERATION: err_str = "GL_INVALID_OPERATION"; break;
			case GL_STACK_OVERFLOW:    err_str = "GL_STACK_OVERFLOW";    break;
			case GL_STACK_UNDERFLOW:   err_str = "GL_STACK_UNDERFLOW";   break;
			case GL_OUT_OF_MEMORY:     err_str = "GL_OUT_OF_MEMORY";     break;
			}

			xen::log(xen::LogLevel::ERROR, file, line, "OpenGl Error %i occured: %s", err, err_str);
		}

		template<typename T>
		T checkGl(T in, u32 line, const char* file){
			checkGl(line, file);
			return in;
		}
	}
}

#endif
