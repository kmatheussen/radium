/* Copyright 2012 Kjetil S. Matheussen

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */



#if defined(FOR_LINUX) || defined(FOR_MACOSX)



#if defined(FOR_LINUX)


#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <link.h>
#include <stdio.h>


#define PACKAGE 1 // workaround for bug in libbfd
#define PACKAGE_VERSION 1 // workaround for bug in libbfd
#include "backtrace-symbols.c"

#elif defined(FOR_MACOSX)

#  include <execinfo.h> // haven't got backtrace-symbols.c to compile on OSX, so we use the original versions of backtrace() instead, which doesn't include line numbers.

#endif

#include <time.h>

#include <sys/time.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <stdbool.h>


#include "../common/nsmtracker.h"
#include "../common/threading.h"

#include "../audio/Juce_plugins_proc.h"

#include "crashreporter_proc.h"


#if 0
static int get_ms(void) {
    struct timeval now;
    
    if (gettimeofday(&now, NULL) != 0)
      return 0.0;

    return now.tv_sec*1000 + now.tv_usec / 1000.0;
}
#endif


static bool crash_already_reported(void){
  static pthread_t *reported_threads = NULL;
  static int num_threads=0;

  if (num_threads > 98)
    return true;
  
  if (reported_threads == NULL){
	  reported_threads = calloc(100, sizeof(pthread_t));
  }  
  
  pthread_t this_thread = pthread_self();

  int i;
  for(i=0;i<num_threads;i++){
    if(reported_threads[i]==this_thread)
      return true;
  }

  reported_threads[num_threads] = this_thread;
  num_threads++;

  return false;
}

void run_main_loop(void);


void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, enum Crash_Type crash_type, double time){
#define NUM_LINES 100

  char **strings;

#if 1 //defined(FOR_LINUX)
  void *buffer[NUM_LINES];
  int nptrs = backtrace(buffer, NUM_LINES);
  
  strings = backtrace_symbols(buffer, nptrs);
  
#elif 0 //defined(FOR_MACOSX)
  int nptrs = 1;
  strings = calloc(2,sizeof(char*));
  strings[0]=(char*)JUCE_get_backtrace(); // No point. Same as calling backtrace_symbols. I had hoped that it contained line numbers. Can not get the 'atos' program to work either.
  strings[1]=NULL;
#endif

  
  
  if (strings != NULL) {
    CRASHREPORTER_send_message(additional_information, (const char**)strings,nptrs,crash_type,time);
  } else {
    const char *message="no backtrace availabe\n";
    CRASHREPORTER_send_message(additional_information, &message,1,crash_type,time);
  }
}

//static void crash(int sig){
static void crash(int sig, siginfo_t *siginfo, void *secret) {

  //fprintf(stderr,"\n\nCrashreporter got signal %d. genpid: %d, this pid: %d. Sending message to external process\n\n",sig,(int)siginfo->si_pid,(int)getpid());

  
  static int num_crash_reports=0;

  static double start_time = -1.0;
  double now_time = TIME_get_ms();

  if(start_time<=0.0)
    start_time=now_time;

  if(crash_already_reported()==false){

    if(fork()==0){
      char temp[64];
      snprintf(temp,63,"signum: %d\n", sig);
      CRASHREPORTER_send_message_with_backtrace(temp, CT_CRASH,now_time);
      num_crash_reports++;

      abort();
    }
  }

#if defined(FOR_LINUX)
  msleep(1000*3);
#else
  msleep(500); // Just wait half second on mac.
#endif
  
  CRASHREPORTER_close();

#if defined(FOR_LINUX)
   
  _Exit(1); // We don't get system crashlog on OSX when doing this.

  if (0)
    msleep(num_crash_reports); // fix compilation error, clang 15

#else
  
  abort();
  
  if((now_time-start_time) > 3000 || num_crash_reports>3){
    CRASHREPORTER_close();
    abort();
  }
#endif
  
  /*
  if(num_crash_reports>10)
    pthread_exit(NULL);
  */
}

static void setup_callstack_signal_handler(int signal) {
  struct sigaction sa;
  sigfillset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;
  sa.sa_sigaction = crash;
  //sa.sa_handler = crash;
  sigaction(signal, &sa, NULL);	
}

void CRASHREPORTER_posix_init(void){
  setup_callstack_signal_handler(SIGSEGV);
  setup_callstack_signal_handler(SIGFPE);

  setup_callstack_signal_handler(SIGINT);
  setup_callstack_signal_handler(SIGILL);

  // Commented out since NSM terminates clients like this. I guess we should probably not catch SIGTERM here anyway, so just remove it whether NSM is used or not.
  //setup_callstack_signal_handler(SIGTERM);

#if !defined(FOR_MACOSX) // Need to call abort() in order to create system crash log.
  setup_callstack_signal_handler(SIGABRT);
#endif
  
  //signal(SIGSEGV,crash);
  //signal(SIGFPE,crash);
  //signal(SIGPIPE,crash);
}


#endif // defined(FOR_LINUX)
// || defined(FOR_MACOSX)
