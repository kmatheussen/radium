/* Copyright 2003 Kjetil S. Matheussen

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

#include <errno.h>
#ifndef FOR_WINDOWS
#include <sys/time.h>
#endif
#include <string.h>
#include <stdint.h>
#include <pthread.h>

#ifndef FOR_WINDOWS
#include <time.h>
#include <unistd.h>
#endif


#ifdef FOR_WINDOWS
#  include <windows.h>
#endif

#include "../common/nsmtracker.h"
#include "../common/OS_Player_proc.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
#include "posix_Player_proc.h"

#define USE_POSIX_PLAYER 0



// Try to call PlayerTask 1200 times a second.
//#define PLAYERTASKFREQ 44100
#define PLAYERTASKFREQ 1200

pthread_t playerthread={0};

bool doexit=false;

extern struct Root *root;
extern PlayerClass *pc;

static bool isplaying=false;

//#include "google/profiler.h"

#if USE_POSIX_PLAYER

static void *posix_PlayerThread(void *arg){
  // TODO: Fix windows
  int64_t newtime;
  int64_t lasttime=0;

#ifdef _FOR_WINDOWS
  if(SetThreadPriority(GetCurrentThread(),THREAD_PRIORITY_TIME_CRITICAL)==false)
    RError("Could not set high priority for player thread.\n");
#endif

  while(doexit==false){
    struct timeval tv;


#if defined(__linux__) || defined(FOR_MACOSX)
    struct timespec req={0,(1000*1000*1000) / PLAYERTASKFREQ};
    nanosleep(&req,NULL);
#endif

#ifdef FOR_WINDOWS
    //usleep(40*1000);
    Sleep(1);
#endif

    gettimeofday(&tv,NULL);
    //newtime=(((tv.tv_sec*1000000+tv.tv_usec)/100)*PFREQ)/10000;
    newtime=tv.tv_sec*(1000*1000);
    newtime+=tv.tv_usec;
    newtime*=pc->pfreq;
    newtime/=(1000*1000);

    //printf("Gakk player %d\n",getpid());

    //printf("newtime: %d, lasttime: %d, -: %d\n",(int)newtime,(int)lasttime,(int)(newtime-lasttime));
    //fflush(stdout);
    PlayerTask(newtime-lasttime);

    lasttime=newtime;
  }

  //ProfilerStop();

  return NULL;
}
#endif // USE_POSIX_PLAYER


void posix_EndPlayer(void){
  doexit=true;
#if USE_POSIX_PLAYER
  pthread_join(playerthread,NULL);
#endif
}

bool posix_InitPlayer(void){

#if !USE_POSIX_PLAYER

  return true; // Using jack now.

#else
  
  if(pthread_create(&playerthread,NULL,posix_PlayerThread,NULL)!=0){
    fprintf(stderr,"Could not create player\n");
    return false;
  } 

#if defined(__linux__) || defined(FOR_MACOSX)
  {
    struct sched_param rtparam;
    int x;
    
    memset (&rtparam, 0, sizeof (rtparam));
    rtparam.sched_priority = 1;
    
    if ((x = pthread_setschedparam (playerthread, SCHED_FIFO, &rtparam)) != 0) {
      RWarning("cannot set thread to real-time priority (FIFO/%d) (%d: %s)", rtparam.sched_priority, x, strerror (errno));
    }
  }
#endif

  return true;

#endif // USE_POSIX_PLAYER

}

void StartPlayer(void){
  isplaying=true;
}

void StopPlayer(void){
  isplaying=false;
}

void PausePlayer(void){
}

void StopPausePlayer(void){
}


void OS_WaitForAShortTime(int milliseconds){
#ifdef FOR_WINDOWS
  Sleep(milliseconds);
#else
  usleep(milliseconds*1000);
#endif
}


