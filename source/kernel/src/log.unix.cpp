////////////////////////////////////////////////////////////////////////////
///                      Part of Xeno Engine                             ///
////////////////////////////////////////////////////////////////////////////
/// \brief Contains platform specific implementation of kernel logging functions
///
/// \ingroup kernel
////////////////////////////////////////////////////////////////////////////

#ifndef XEN_KERNEL_LOG_UNIX_CPP
#define XEN_KERNEL_LOG_UNIX_CPP

#include "log.hxx"
#include <xen/kernel/threads.hpp>
#include <xen/core/String.hpp>
#include <xen/core/memory/utilities.hpp>
#include <xen/graphics/Color.hpp>

#include <cstdio>

#include <unistd.h>
#include <termios.h>
#include <term.h>
#include <curses.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#include <pthread.h>

struct LogData {
	pthread_mutex_t print_mutex;
};
LogData log_data;

/// \brief Converts an rgb color to the closest ansi color between 0 and 256
uint ansiColor(xen::Color c){
	uint r = ((uint)c.r) * 6 / 256;
	uint g = ((uint)c.g) * 6 / 256;
	uint b = ((uint)c.b) * 6 / 256;

  return 16 + 36 * r + 6 * g + b;
}


bool xke::initLogSubsystem(){
	pthread_mutex_init(&log_data.print_mutex, nullptr);

	int ret;
	if(setupterm("xterm-256color", STDOUT_FILENO, &ret) != OK){
		printf("Failed to setup terminal\n");
		return false;
	}


	return true;
}

void xke::printLogMsgToStdio(const xen::LogMessage& lm){

	if(lm.level < xen::LogLevel::DEBUG){
		return;
	}

	// :TODO: we should really determine if the terminal supports 256 colors
	// before using them
	// see: http://docs.ros.org/kinetic/api/rosmon/html/terminal_8cpp_source.html
	static const char* str_setaf = tigetstr("setaf");
	static const char* str_setab = tigetstr("setab");

	xen::Color  level_foreground;
	xen::Color  level_background;
	const char* level_name;

	switch(lm.level){
	case xen::LogLevel::TRACE:
		level_name = "TRACE";
		level_foreground = xen::Color{255, 255, 255};
		level_background = xen::Color{ 70,  80,  90};
		break;
	case xen::LogLevel::DEBUG:
		level_name = "DEBUG";
		level_foreground = xen::Color{255, 255, 255};
		level_background = xen::Color{ 30,  30,  30};
		break;
	case xen::LogLevel::INFO:
		level_name = "INFO ";
	  level_foreground = xen::Color{  0,   0,   0};
		level_background = xen::Color{  0, 255, 255};
		break;
	case xen::LogLevel::DONE:
		level_name = "DONE ";
	  level_foreground = xen::Color{  0,   0,   0};
		level_background = xen::Color{130, 255,  40};
		break;
	case xen::LogLevel::WARN:
		level_name = "WARN ";
		level_foreground = xen::Color{  0,   0,   0};
		level_background = xen::Color{255, 150,   0};
		break;
	case xen::LogLevel::ERROR:
		level_name = "ERROR";
		level_foreground = xen::Color{255, 255, 255};
		level_background = xen::Color{255,   0,   0};
		break;
	case xen::LogLevel::FATAL_DEV:
	case xen::LogLevel::FATAL:
		level_name       = "FATAL";
		level_foreground = xen::Color{255, 255, 255};
		level_background = xen::Color{100,   0,   0};
	}

	char date_buffer[256];
	formatDateTime(lm.time,
	               date_buffer, XenArrayLength(date_buffer),
	               "%H:%M:%S");


	const char* file_name_start = nullptr;

	// chop off known prefixes to file_name
	if       ((file_name_start = xen::findFirst(lm.file, "include/xen"))){
		file_name_start = &file_name_start[8];
	} else if((file_name_start = xen::findFirst(lm.file, "impl/xen"))){
		file_name_start = &file_name_start[5];
	} else if((file_name_start = xen::findFirst(lm.file, "xeno-engine/source/"))){
		file_name_start = &file_name_start[19];
	} else {
		file_name_start = lm.file;
	}
	// trim file name to a max length

	constexpr s32 FILE_NAME_LENGTH = 24;
	const char* file_name = file_name_start;
	while(*file_name != '\0') { ++file_name; }
	file_name = &file_name[-FILE_NAME_LENGTH];
	if(file_name_start > file_name){
		file_name = file_name_start;
	}

	struct winsize term_size;
	ioctl(STDOUT_FILENO, TIOCGWINSZ, &term_size);

	constexpr u32 LEVEL_WIDTH = 5 + 3 + 2 + 3 + 8 + 3 + FILE_NAME_LENGTH + 4 + 1;

	pthread_mutex_lock(&log_data.print_mutex);

	putp(tiparm(str_setab, ansiColor(level_background)));
	putp(tiparm(str_setaf, ansiColor(level_foreground)));

	printf("%5s |", level_name);

	if(lm.thread == xen::BAD_THREAD_INDEX){
		printf(" ?? |");
	} else {
		printf(" %2lu |", lm.thread);
	}
	printf(" %8s | %*s%4u ",
	       date_buffer,
	       FILE_NAME_LENGTH, file_name, lm.line_number);

	printf("\033[0m ");

	if(term_size.ws_col < LEVEL_WIDTH + 25){
		// Then just print the string and let it wrap
		printf("%s\n", lm.message);
	} else {
		// Otherwise pretty print it with auto-wrapping onto new lines prefixed by color blocks
		u32 chars_written = LEVEL_WIDTH+1;
		const char* msg = lm.message;

		while(*msg != '\0'){
			putchar(*msg);
			++chars_written;
			char char_written = *msg;
			++msg;
			if(chars_written >= term_size.ws_col || char_written == '\n'){
				if(char_written != '\n'){ printf("\n"); }
				putp(tiparm(str_setab, ansiColor(level_background)));
				putp(tiparm(str_setaf, ansiColor(level_foreground)));
				printf("%-*s", LEVEL_WIDTH, "      |    |          |");
				chars_written = LEVEL_WIDTH+1;
				printf("\033[0m ");

				if(char_written != '\n'){
					// if we auto-wrapped trim leading white space following the wrap
					while(*msg == ' ' || *msg == '\t'){
						++msg;
					}
				}
			}
		}
	}

	putchar('\n');

	pthread_mutex_unlock(&log_data.print_mutex);
}

#endif
