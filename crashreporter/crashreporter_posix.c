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

#if defined(FOR_LINUX)
// || defined(FOR_MACOSX)

#define PACKAGE 1 // workaround for bug in libbfd
#define PACKAGE_VERSION 1 // workaround for bug in libbfd
#include "backtrace-symbols.c"

#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <stdbool.h>

#include "../common/nsmtracker.h"
#include "../common/threading.h"

#include "crashreporter_proc.h"


static double get_ms(void){
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1000.0 + ((double)ts.tv_nsec) / 1000000.0;
}


static bool crash_already_reported(void){
  static pthread_t reported_threads[100];
  static int num_threads=0;

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

void CRASHREPORTER_send_message_with_backtrace(const char *additional_information, bool is_crash){
#define NUM_LINES 100
      
  void *buffer[NUM_LINES];
  char **strings;
  int nptrs = backtrace(buffer, NUM_LINES);
  
  strings = backtrace_symbols(buffer, nptrs);
  
  if (strings != NULL) {
    CRASHREPORTER_send_message(additional_information, (const char**)strings,nptrs,is_crash);
  } else {
    const char *message="no backtrace availabe\n";
    CRASHREPORTER_send_message(additional_information, &message,1,true);
  }
}

//static void crash(int sig){
static void crash(int sig, siginfo_t *siginfo, void *secret) {

  //fprintf(stderr,"\n\nCrashreporter got signal %d. genpid: %d, this pid: %d. Sending message to external process\n\n",sig,(int)siginfo->si_pid,(int)getpid());

  static int num_crash_reports=0;

  static double start_time = -1.0;
  double now_time = get_ms();

  if(start_time<=0.0)
    start_time=now_time;

  if(crash_already_reported()==false){

    if(fork()==0){
      CRASHREPORTER_send_message_with_backtrace("", true);
      num_crash_reports++;

      abort();
    }
  }

  if((now_time-start_time) > 3000 || num_crash_reports>3){
    CRASHREPORTER_close();
    abort();
  }

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
  
  //signal(SIGSEGV,crash);
  //signal(SIGFPE,crash);
  //signal(SIGPIPE,crash);
}


#endif // defined(FOR_LINUX)
// || defined(FOR_MACOSX)
