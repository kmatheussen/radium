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
#include <sys/time.h>
#include <string.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>


#include "../common/nsmtracker.h"
#include "../common/OS_Player_proc.h"
#include "../common/player_proc.h"
#include "../common/playerclass.h"
#include "posix_Player_proc.h"


// Try to call PlayerTask 1200 times a second.
#define PLAYERTASKFREQ 1200

pthread_t playerthread={0};

bool doexit=false;

extern struct Root *root;

static bool isplaying=false;

//#include "google/profiler.h"

static void *posix_PlayerThread(void *arg){
  int64_t newtime;
  int64_t lasttime=0;

  while(doexit==false){
    struct timeval tv;
    struct timespec req={0,1000000000 / PLAYERTASKFREQ};

    nanosleep(&req,NULL);

    gettimeofday(&tv,NULL);
    //newtime=(((tv.tv_sec*1000000+tv.tv_usec)/100)*PFREQ)/10000;
    newtime=tv.tv_sec*1000000;
    newtime+=tv.tv_usec;
    newtime*=PFREQ;
    newtime/=1000000;

    //printf("newtime: %lld, lasttime: %lld, -: %lld\n",newtime,lasttime,newtime-lasttime);
    PlayerTask(newtime-lasttime);

    lasttime=newtime;
  }

  //ProfilerStop();

  return NULL;
}

void posix_EndPlayer(void){
  doexit=true;
  pthread_join(playerthread,NULL);
}

bool posix_InitPlayer(void){

  if(pthread_create(&playerthread,NULL,posix_PlayerThread,NULL)!=0){
    fprintf(stderr,"Could not create player\n");
    return false;
  } 

  {
    struct sched_param rtparam;
    int x;
    
    memset (&rtparam, 0, sizeof (rtparam));
    rtparam.sched_priority = 1;
    
    if ((x = pthread_setschedparam (playerthread, SCHED_FIFO, &rtparam)) != 0) {
      fprintf(stderr,"cannot set thread to real-time priority (FIFO/%d) (%d: %s)", rtparam.sched_priority, x, strerror (errno));
    }
  }

  return true;
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
  usleep(milliseconds*1000);
}

