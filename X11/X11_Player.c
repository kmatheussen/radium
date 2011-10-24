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

#include <X11/Xlib.h>

#include "../common/nsmtracker.h"

#include "X11.h"

#include <pthread.h>
#include <unistd.h>

#include "../common/OS_Player_proc.h"

#include "../common/player_proc.h"
#include "../common/playerclass.h"

#include "X11_Player_proc.h"

// Try to call PlayerTask 1200 times a second.
#define PLAYERTASKFREQ 1200

pthread_t playerthread={0};

bool doexit=false;

extern struct Root *root;

static bool isplaying=false;

void *X11_PlayerThread(void *arg){
  long long newtime;
  long long lasttime=0;

  XEvent message;

  message.xclient.type = ClientMessage;
  message.xclient.format=32;


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

#if 0
    if(isplaying==true){
      message.xclient.window = root->song->tracker_windows->os_visual->window;
      message.xclient.data.l[0]=X11EVENT_UPDATESONGPOS;
      
      XSendEvent(
		 x11_display,
		 message.xclient.window,
		 True,
		 NoEventMask,
		 &message
		 );
    }
#endif
  }
  return NULL;
}

void X11_EndPlayer(void){
  doexit=true;
  pthread_join(playerthread,NULL);
}

bool X11_InitPlayer(void){

  if(pthread_create(&playerthread,NULL,X11_PlayerThread,NULL)!=0){
    fprintf(stderr,"Could not create player\n");
    return false;
  } 

#if 0
  {
    struct sched_param rtparam;
    int x;
    
    memset (&rtparam, 0, sizeof (rtparam));
    rtparam.sched_priority = 10;
    
    //system("/usr/bin/givertcap");
    
    if ((x = pthread_setschedparam (playerthread, SCHED_FIFO, &rtparam)) != 0) {
      fprintf(stderr,"cannot set thread to real-time priority (FIFO/%d) (%d: %s)", rtparam.sched_priority, x, strerror (errno));
    }
  }
#endif

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

